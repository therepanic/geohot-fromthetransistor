import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Stream;

public class Linker {

    public static void main(String[] args) throws IOException {
        List<byte[]> binaries = collectBinariesBytes();
        Map<Placement, Long> placementOffset = new HashMap<>();
        Set<String> addedRelocs = new HashSet<>();
        byte[] outText = buildOutText(binaries, placementOffset, addedRelocs);
        byte[] outData = buildOutData(binaries, placementOffset, addedRelocs);
        int bssSize = (int) getBssSize(binaries);
        List<SectionBase> fileBases = new ArrayList<>();
        long textCursor = 0;
        long dataCursor = 0;
        long bssCursor = 0;
        int fileIdx = 0;
        for (byte[] data : binaries) {
            fileBases.add(new SectionBase(textCursor, dataCursor, bssCursor));

            textCursor += getTextSize(data);
            dataCursor += getDataSize(data);
            bssCursor += getBssSize(data);

            fileIdx++;
        }

        List<Relocation> relocs = new ArrayList<>();
        fileIdx = 0;
        for (byte[] data : binaries) {
            collectReloc(data, relocs, fileBases.get(fileIdx));
            fileIdx++;
        }
        long TEXT_BASE = 0x1000;
        long DATA_BASE = alignUp(TEXT_BASE + outText.length, 0x1000);
        long BSS_BASE = DATA_BASE + outData.length;
        for (Relocation relo : relocs) {
            int patchPos = (int) relo.rel().rOffset();
            long symbolOffset = findSymbolOffset(relo.name(), placementOffset);
            long symbolAddr = symbolToAbsoluteAddr(relo.name(), symbolOffset, TEXT_BASE, DATA_BASE, BSS_BASE, placementOffset);
            int type = (int) (relo.rel().rInfo() & 0xFF);
            byte[] targetBuf = relo.isTextReloc() ? outText : outData;
            switch (type) {
                case RelocType.R_ARM_CALL, RelocType.R_ARM_JUMP24 -> {
                    long instr = readUInt32LE(targetBuf, patchPos);
                    long pcBase = relo.isTextReloc() ? TEXT_BASE : DATA_BASE;
                    long pcAddr = pcBase + patchPos + 8;
                    long offset = (symbolAddr - pcAddr) / 4;
                    instr = (instr & 0xFF000000L) | (offset & 0x00FFFFFF);
                    writeUInt32LE(targetBuf, patchPos, (int) instr);
                }
                case RelocType.R_ARM_ABS32 -> {
                    writeUInt32LE(targetBuf, patchPos, symbolAddr);
                }
                case RelocType.R_ARM_MOVW_ABS_NC, RelocType.R_ARM_MOVT_ABS -> {
                    int value16 = (int) (symbolAddr >> (type == RelocType.R_ARM_MOVT_ABS ? 16 : 0)) & 0xFFFF;
                    int imm4 = (value16 >> 12) & 0xF;
                    int imm12 = value16 & 0xFFF;
                    long instr = readUInt32LE(targetBuf, patchPos);
                    instr &= 0xFFF0F000L;
                    instr |= (imm4 << 16);
                    instr |= imm12;
                    writeUInt32LE(targetBuf, patchPos, (int) instr);
                }
                case RelocType.R_ARM_REL32 -> {
                    int addend = (int) readUInt32LE(targetBuf, patchPos);
                    long pcAddr;
                    if (relo.isTextReloc()) {
                        pcAddr = TEXT_BASE + patchPos;
                    } else {
                        pcAddr = DATA_BASE + patchPos;
                    }
                    long offset = (symbolAddr + addend) - pcAddr;
                    writeUInt32LE(targetBuf, patchPos, (int) offset);
                }
            }
        }
        int textOffset = (int) TEXT_BASE;
        int dataOffset = (int) DATA_BASE;
        byte[] output = new byte[dataOffset + outData.length];
        int BASE_VADDR = 0x10000;
        int PAGE = 0x1000;
        fillMagic(output);
        fillElfHeader(output, BASE_VADDR, textOffset, placementOffset);
        insertSegment(output, textOffset, outText); //text
        insertSegment(output, dataOffset, outData); //data
        insertTextProgramHeader(output, 52, textOffset, BASE_VADDR, outText, PAGE);
        insertDataProgramHeader(output, 52 + 32, dataOffset, BASE_VADDR, outData, PAGE, bssSize);
        // save output
        Files.write(Paths.get("output.elf"), output);
    }

    private static void fillMagic(byte[] output) {
        output[0] = 0x7F; output[1] = 'E'; output[2] = 'L'; output[3] = 'F';
        output[4] = 1;
        output[5] = 1;
        output[6] = 1;
        output[7] = 0x00;
        output[8] = 0;
    }

    private static void fillElfHeader(byte[] output, int BASE_VADDR, int textOffset, Map<Placement, Long> placementOffset) {
        int offset = 16;
        long startOff = findSymbolOffset("_start", placementOffset);
        writeUInt16LE(output, offset, 2); offset += 2; //type
        writeUInt16LE(output, offset, 40); offset += 2; //machine
        writeUInt32LE(output, offset, 1); offset += 4; //version
        writeUInt32LE(output, offset, BASE_VADDR + textOffset + startOff); offset += 4; //entry
        writeUInt32LE(output, offset, 52); offset += 4; //phoff
        writeUInt32LE(output, offset, Elf32ShdrType.SHT_NULL); offset += 4; //shoff
        writeUInt32LE(output, offset, 0x05000000); offset += 4; //flags
        writeUInt16LE(output, offset, 52); offset += 2; //ehsize
        writeUInt16LE(output, offset, 32); offset += 2; //phentsize
        writeUInt16LE(output, offset, 2); offset += 2; //phnum
        writeUInt16LE(output, offset, Elf32ShdrType.SHT_NULL); offset += 2; //shentsize
        writeUInt16LE(output, offset, Elf32ShdrType.SHT_NULL); offset += 2; //shnum
        writeUInt16LE(output, offset, Elf32ShdrType.SHT_NULL); offset += 2; //shstrndx
    }

    private static void insertSegment(byte[] output, int offset, byte[] data) {
        int end = offset + data.length;
        for (int i = offset; i < end; i++) {
            output[i] = data[i - offset];
        }
    }

    private static void insertTextProgramHeader(byte[] output, int offset, int textOffset, int BASE_VADDR, byte[] textData, int align) {
        writeUInt32LE(output, offset, Elf32PhdrType.PT_LOAD); offset += 4; //type
        writeUInt32LE(output, offset, textOffset); offset += 4; //offset
        writeUInt32LE(output, offset, BASE_VADDR + textOffset); offset += 4; //vaddr
        writeUInt32LE(output, offset, BASE_VADDR + textOffset); offset += 4; //paddr
        writeUInt32LE(output, offset, textData.length); offset += 4; //pfilesz
        writeUInt32LE(output, offset, textData.length); offset += 4; //pmemsz
        writeUInt32LE(output, offset, Elf32PhdrPermission.PF_X | Elf32PhdrPermission.PF_R); offset += 4; //pflags
        writeUInt32LE(output, offset, align); offset += 4; //align
    }

    private static void insertDataProgramHeader(byte[] output, int offset, int dataOffset, int BASE_VADDR, byte[] dataData, int align, long bssSize) {
        writeUInt32LE(output, offset, Elf32PhdrType.PT_LOAD); offset += 4; //type
        writeUInt32LE(output, offset, dataOffset); offset += 4; //offset
        writeUInt32LE(output, offset, BASE_VADDR + dataOffset); offset += 4; //vaddr
        writeUInt32LE(output, offset, BASE_VADDR + dataOffset); offset += 4; //paddr
        writeUInt32LE(output, offset, dataData.length); offset += 4; //pfilesz
        writeUInt32LE(output, offset, dataData.length + bssSize); offset += 4; //pmemsz
        writeUInt32LE(output, offset, Elf32PhdrPermission.PF_W | Elf32PhdrPermission.PF_R); offset += 4; //pflags
        writeUInt32LE(output, offset, align); offset += 4; //align
    }

    private static Elf32Ehdr parseHeader(byte[] data) {
        int offset = 16;
        int eType = readUInt16LE(data, offset); offset += 2;
        int eMachine = readUInt16LE(data, offset); offset += 2;
        long eVersion = readUInt32LE(data, offset); offset += 4;
        long eEntry = readUInt32LE(data, offset); offset += 4;
        long ePhoff = readUInt32LE(data, offset); offset += 4;
        long eShoff = readUInt32LE(data, offset); offset += 4;
        long eFlags = readUInt32LE(data, offset); offset += 4;
        int eEhsize = readUInt16LE(data, offset); offset += 2;
        int ePhentsize = readUInt16LE(data, offset); offset += 2;
        int ePhnum = readUInt16LE(data, offset); offset += 2;
        int eShentsize = readUInt16LE(data, offset); offset += 2;
        int eShnum = readUInt16LE(data, offset); offset += 2;
        int eShstrndx = readUInt16LE(data, offset);
        return new Elf32Ehdr(eType, eMachine, eVersion, eEntry, ePhoff, eShoff, eFlags,
                eEhsize, ePhentsize, ePhnum, eShentsize, eShnum, eShstrndx);
    }

    private static Elf32Shdr[] parseSections(byte[] data, Elf32Ehdr header) {
        Elf32Shdr[] sections = new Elf32Shdr[header.eShnum()];
        for (int i = 0; i < header.eShnum(); i++) {
            int offset = (int) header.eShoff() + i * header.eShentsize();
            long shName = readUInt32LE(data, offset); offset += 4;
            long shType = readUInt32LE(data, offset); offset += 4;
            long shFlags = readUInt32LE(data, offset); offset += 4;
            long shAddr = readUInt32LE(data, offset); offset += 4;
            long shOffset = readUInt32LE(data, offset); offset += 4;
            long shSize = readUInt32LE(data, offset); offset += 4;
            long shLink = readUInt32LE(data, offset); offset += 4;
            long shInfo = readUInt32LE(data, offset); offset += 4;
            long shAddralign = readUInt32LE(data, offset); offset += 4;
            long shEntsize = readUInt32LE(data, offset);
            sections[i] = new Elf32Shdr(shName, shType, shFlags, shAddr, shOffset, shSize,
                    shLink, shInfo, shAddralign, shEntsize);
        }
        return sections;
    }

    private static long getTextSize(byte[] data) {
        Elf32Ehdr header = parseHeader(data);
        Elf32Shdr[] sections = parseSections(data, header);
        long size = 0;
        for (int shndx = 0; shndx < sections.length; shndx++) {
            Elf32Shdr section = sections[shndx];
            // if it .text
            if (section.shType() == Elf32ShdrType.SHT_PROGBITS && (section.shFlags() & Elf32ShdrFlag.SHF_ALLOC) != 0
                    && (section.shFlags() & Elf32ShdrFlag.SHF_EXECINSTR) != 0) {
                size += section.shSize();
            }
        }
        return size;
    }

    private static long getDataSize(byte[] data) {
        Elf32Ehdr header = parseHeader(data);
        Elf32Shdr[] sections = parseSections(data, header);
        long size = 0;
        for (int shndx = 0; shndx < sections.length; shndx++) {
            Elf32Shdr section = sections[shndx];
            // if it .data
            if (section.shType() == Elf32ShdrType.SHT_PROGBITS && (section.shFlags() & Elf32ShdrFlag.SHF_ALLOC) != 0
                    && (section.shFlags() & Elf32ShdrFlag.SHF_WRITE) != 0) {
                size += section.shSize();
            }
        }
        return size;
    }

    private static long getBssSize(byte[] data) {
        Elf32Ehdr header = parseHeader(data);
        Elf32Shdr[] sections = parseSections(data, header);
        long size = 0;
        for (int shndx = 0; shndx < sections.length; shndx++) {
            Elf32Shdr section = sections[shndx];
            // if it .bss
            if (section.shType() == Elf32ShdrType.SHT_NOBITS && (section.shFlags() & Elf32ShdrFlag.SHF_ALLOC) != 0
                    && (section.shFlags() & Elf32ShdrFlag.SHF_WRITE) != 0) {
                size += section.shSize();
            }
        }
        return size;
    }

    private static void fillPlacementOffset(byte[] data, Map<Placement, Long> placementOffset, Set<String> addedRelocs, int fileIdx) {
        Elf32Ehdr header = parseHeader(data);
        Elf32Shdr[] sections = parseSections(data, header);
        for (Elf32Shdr section : sections) {
            // if it symtab
            if (section.shType() == Elf32ShdrType.SHT_SYMTAB) {
                byte[] strtab = readSectionBytes(data, sections[(int) section.shLink()]);
                for (int i = 0; i < section.shSize() / section.shEntsize(); i++) {
                    int offset = (int) (section.shOffset() + i * section.shEntsize());

                    long stName = readUInt32LE(data, offset); offset += 4;
                    long stValue = readUInt32LE(data, offset); offset += 4;
                    long stSize = readUInt32LE(data, offset); offset += 4;
                    long stInfo = data[offset] & 0xFF; offset++;
                    long stOther = data[offset] & 0xFF; offset++;
                    int stShndx = readUInt16LE(data, offset); offset += 2;

                    String name = readCString(strtab, (int) stName);
                    if (name.startsWith("$")) continue;

                    if (stShndx != Elf32ShdrIndex.SHN_UNDEF) {
                        if (stShndx >= 0xFF00 || stShndx >= sections.length || stName == 0 || addedRelocs.contains(name)) continue;
                        addedRelocs.add(name);
                        Elf32Shdr symSection = sections[stShndx];
                        SectionType type = getSectionType(symSection);
                        Placement placement = new Placement(stShndx, fileIdx, name, type);
                        placementOffset.put(placement, stValue);
                    }
                }
            }
        }
    }

    private static int buildOutText(byte[] data, byte[] out, int cursor, Map<Placement, Long> placementOffset, int fileIdx) {
        Elf32Ehdr header = parseHeader(data);
        Elf32Shdr[] sections = parseSections(data, header);
        for (int shndx = 0; shndx < sections.length; shndx++) {
            Elf32Shdr section = sections[shndx];
            // if it .text
            if (section.shType() == Elf32ShdrType.SHT_PROGBITS && (section.shFlags() & Elf32ShdrFlag.SHF_ALLOC) != 0
                    && (section.shFlags() & Elf32ShdrFlag.SHF_EXECINSTR) != 0) {
                for (var entry : placementOffset.entrySet()) {
                    if (entry.getKey().ownerShndx() == shndx && entry.getKey().fileIdx() == fileIdx) {
                        entry.setValue(entry.getValue() + cursor);
                    }
                }
                int offset = (int) section.shOffset();
                for (int i = offset; i < section.shSize() + offset; i++, cursor++) {
                    out[cursor] = data[i];
                }
            }
        }
        return cursor;
    }

    private static int buildOutData(byte[] data, byte[] out, int cursor, Map<Placement, Long> placementOffset, int fileIdx) {
        Elf32Ehdr header = parseHeader(data);
        Elf32Shdr[] sections = parseSections(data, header);
        for (int shndx = 0; shndx < sections.length; shndx++) {
            Elf32Shdr section = sections[shndx];
            // if it .data
            if (section.shType() == Elf32ShdrType.SHT_PROGBITS && (section.shFlags() & Elf32ShdrFlag.SHF_ALLOC) != 0
                    && (section.shFlags() & Elf32ShdrFlag.SHF_WRITE) != 0) {
                for (var entry : placementOffset.entrySet()) {
                    if (entry.getKey().ownerShndx() == shndx && entry.getKey().fileIdx() == fileIdx) {
                        entry.setValue(entry.getValue() + cursor);
                    }
                }
                int offset = (int) section.shOffset();
                for (int i = offset; i < section.shSize() + offset; i++, cursor++) {
                    out[cursor] = data[i];
                }
            }
        }
        return cursor;
    }

    private static long getTextsSize(List<byte[]> binaries) throws IOException {
        long textSize = 0;
        for (byte[] data : binaries) {
            textSize += getTextSize(data);
        }
        return textSize;
    }

    private static long getDataSize(List<byte[]> binaries) throws IOException {
        long dataSize = 0;
        for (byte[] data : binaries) {
            dataSize += getDataSize(data);
        }
        return dataSize;
    }

    private static long getBssSize(List<byte[]> binaries) throws IOException {
        long bssSize = 0;
        for (byte[] data : binaries) {
            bssSize += getBssSize(data);
        }
        return bssSize;
    }

    private static byte[] buildOutText(List<byte[]> binaries, Map<Placement, Long> placementOffset, Set<String> addedRelocs) throws IOException {
        long textSize = getTextsSize(binaries);
        byte[] outText = new byte[(int) textSize];
        int fileIdx = 0;
        for (byte[] data : binaries) {
            fillPlacementOffset(data, placementOffset, addedRelocs, fileIdx);
            fileIdx++;
        }
        fileIdx = 0;
        int cursor = 0;
        for (byte[] data : binaries) {
            cursor = buildOutText(data, outText, cursor, placementOffset, fileIdx);
            fileIdx++;
        }
        return outText;
    }

    private static byte[] buildOutData(List<byte[]> binaries, Map<Placement, Long> placementOffset, Set<String> addedRelocs) throws IOException {
        long dataSize = getDataSize(binaries);
        byte[] outData = new byte[(int) dataSize];
        int fileIdx = 0;
        for (byte[] data : binaries) {
            fillPlacementOffset(data, placementOffset, addedRelocs, fileIdx);
            fileIdx++;
        }
        fileIdx = 0;
        int cursor = 0;
        for (byte[] data : binaries) {
            cursor = buildOutData(data, outData, cursor, placementOffset, fileIdx);
            fileIdx++;
        }
        return outData;
    }

    private static SymbolEntry getSymbols(byte[] data, Elf32Shdr[] sections) {
        for (Elf32Shdr section : sections) {
            // if it symtab
            if (section.shType() == Elf32ShdrType.SHT_SYMTAB) {
                Elf32Sym[] symbols = new Elf32Sym[(int) (section.shSize() / section.shEntsize())];
                for (int i = 0; i < symbols.length; i++) {
                    int offset = (int) (section.shOffset() + i * section.shEntsize());

                    long stName = readUInt32LE(data, offset);
                    offset += 4;
                    long stValue = readUInt32LE(data, offset);
                    offset += 4;
                    long stSize = readUInt32LE(data, offset);
                    offset += 4;
                    long stInfo = data[offset] & 0xFF;
                    offset++;
                    long stOther = data[offset] & 0xFF;
                    offset++;
                    int stShndx = readUInt16LE(data, offset);
                    offset += 2;
                    symbols[i] = new Elf32Sym(stName, stValue, stSize, stInfo, stOther, stShndx);
                }
                return new SymbolEntry(section, symbols);
            }
        }
        throw new IllegalStateException("No .symtab was found");
    }

    private static void collectReloc(byte[] data, List<Relocation> relocs, SectionBase sectionBase) {
        Elf32Ehdr header = parseHeader(data);
        Elf32Shdr[] sections = parseSections(data, header);
        SymbolEntry entry = getSymbols(data, sections);
        byte[] strtab = readSectionBytes(data, sections[(int) entry.section().shLink()]);
        for (Elf32Shdr section : sections) {
            // if it reloc
            if (section.shType() == Elf32ShdrType.SHT_REL) {
                Elf32Shdr targetSection = sections[(int) section.shInfo()];
                for (int i = 0; i < section.shSize() / section.shEntsize(); i++) {
                    int offset = (int) (section.shOffset() + i * section.shEntsize());

                    long rOffset = readUInt32LE(data, offset); offset += 4;
                    long rInfo = readUInt32LE(data, offset);

                    int symIdx = (int) (rInfo >>> 8);
                    Elf32Sym sym = entry.symbols()[symIdx];

                    int type = (int) (rInfo & 0xFF);
                    long realOffset = getSectionBase(targetSection, sectionBase) + rOffset;
                    boolean isTextReloc = (targetSection.shFlags() & Elf32ShdrFlag.SHF_EXECINSTR) != 0;
                    switch (type) {
                        case RelocType.R_ARM_CALL, RelocType.R_ARM_JUMP24, RelocType.R_ARM_ABS32,
                             RelocType.R_ARM_MOVW_ABS_NC, RelocType.R_ARM_MOVT_ABS, RelocType.R_ARM_REL32 ->
                                relocs.add(new Relocation(readCString(strtab, (int) sym.stName()),
                                        new Elf32Rel(realOffset, rInfo), isTextReloc));
                        default -> throw new IllegalStateException("Relocation type " + type + " not support yet");
                    }
                }
            }
        }
    }

    private static long getSectionBase(Elf32Shdr section, SectionBase base) {
        // .text
        if (section.shType() == Elf32ShdrType.SHT_PROGBITS && (section.shFlags() & Elf32ShdrFlag.SHF_ALLOC) != 0
                && (section.shFlags() & Elf32ShdrFlag.SHF_EXECINSTR) != 0) {
            return base.textBase();
        }
        // .data
        if (section.shType() == Elf32ShdrType.SHT_PROGBITS && (section.shFlags() & Elf32ShdrFlag.SHF_ALLOC) != 0
                && (section.shFlags() & Elf32ShdrFlag.SHF_WRITE) != 0) {
            return base.dataBase();
        }
        // .bss
        if (section.shType() == Elf32ShdrType.SHT_NOBITS && (section.shFlags() & Elf32ShdrFlag.SHF_ALLOC) != 0
                && (section.shFlags() & Elf32ShdrFlag.SHF_WRITE) != 0) {
            return base.bssBase();
        }
        return 0;
    }

    private static SectionType getSectionType(Elf32Shdr section) {
        // .text
        if (section.shType() == Elf32ShdrType.SHT_PROGBITS && (section.shFlags() & Elf32ShdrFlag.SHF_ALLOC) != 0
                && (section.shFlags() & Elf32ShdrFlag.SHF_EXECINSTR) != 0) {
            return SectionType.TEXT;
        }
        // .data
        if (section.shType() == Elf32ShdrType.SHT_PROGBITS && (section.shFlags() & Elf32ShdrFlag.SHF_ALLOC) != 0
                && (section.shFlags() & Elf32ShdrFlag.SHF_WRITE) != 0) {
            return SectionType.DATA;
        }
        // .bss
        if (section.shType() == Elf32ShdrType.SHT_NOBITS && (section.shFlags() & Elf32ShdrFlag.SHF_ALLOC) != 0
                && (section.shFlags() & Elf32ShdrFlag.SHF_WRITE) != 0) {
            return SectionType.BSS;
        }
        return SectionType.DATA;
    }

    private static long findSymbolOffset(String name, Map<Placement, Long> placementOffset) {
        for (var entry : placementOffset.entrySet()) {
            if (entry.getKey().symbol().equals(name)) {
                return entry.getValue();
            }
        }
        throw new IllegalStateException("Undefined symbol: " + name);
    }

    private static long symbolToAbsoluteAddr(String name, long offset, long textBase, long dataBase, long bssBase,
                                       Map<Placement, Long> placementOffset) {
        for (var entry : placementOffset.entrySet()) {
            Placement p = entry.getKey();
            if (p.symbol().equals(name)) {
                if (p.sectionType().equals(SectionType.TEXT)) {
                    return textBase + offset;
                } else if (p.sectionType().equals(SectionType.DATA)) {
                    return dataBase + offset;
                } else if (p.sectionType().equals(SectionType.BSS)) {
                    return bssBase + offset;
                }
            }
        }
        throw new IllegalStateException("Undefined symbol: " + name);
    }

    // Helpers
    private static int readUInt16LE(byte[] b, int off) {
        return (b[off] & 0xFF) | ((b[off + 1] & 0xFF) << 8);
    }

    private static long readUInt32LE(byte[] b, int off) {
        return (b[off] & 0xFF)
                | ((long) (b[off + 1] & 0xFF) << 8)
                | ((long) (b[off + 2] & 0xFF) << 16)
                | ((long) (b[off + 3] & 0xFF) << 24);
    }

    private static void writeUInt16LE(byte[] b, int off, int value) {
        b[off] = (byte) (value & 0xFF);
        b[off + 1] = (byte) ((value >>> 8) & 0xFF);
    }

    private static void writeUInt32LE(byte[] b, int off, long value) {
        b[off] = (byte) (value & 0xFF);
        b[off + 1] = (byte) ((value >>> 8)  & 0xFF);
        b[off + 2] = (byte) ((value >>> 16) & 0xFF);
        b[off + 3] = (byte) ((value >>> 24) & 0xFF);
    }

    private static String readCString(byte[] buf, int start) {
        int i = start;
        while (i < buf.length && buf[i] != 0) {
            i++;
        }
        return new String(buf, start, i - start);
    }

    private static byte[] readSectionBytes(byte[] file, Elf32Shdr section) {
        if (section.shType() == Elf32ShdrType.SHT_NOBITS) {
            return new byte[(int) section.shSize()];
        }
        int off = (int) section.shOffset();
        int size = (int) section.shSize();
        byte[] out = new byte[size];
        System.arraycopy(file, off, out, 0, size);
        return out;
    }

    private static List<byte[]> collectBinariesBytes() {
        List<byte[]> binaries = new ArrayList<>();
        try (Stream<Path> paths = Files.walk(Paths.get("binaries")).sorted()) {
            for (Iterator<Path> it = paths.iterator(); it.hasNext();) {
                Path path = it.next();
                if (!Files.isRegularFile(path)) continue;

                byte[] data = Files.readAllBytes(path);
                binaries.add(data);
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return binaries;
    }

    public static long alignUp(long value, long align) {
        return (value + align - 1) & -align;
    }


}
