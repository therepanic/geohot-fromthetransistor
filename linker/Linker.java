import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.stream.Stream;

public class Linker {

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

    private static void fillPlacementOffset(byte[] data, Map<Placement, Long> placementOffset, int fileIdx) {
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

                    if (stShndx != Elf32ShdrIndex.SHN_UNDEF) {
                        if (stShndx <= 0 || stShndx >= sections.length || stName == 0) continue;
                        placementOffset.put(new Placement(stShndx, fileIdx, readCString(strtab, (int) stName)), stValue);
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

    public static void main(String[] args) throws IOException {
        Map<Placement, Long> placementOffset = new HashMap<>();
        int fileIdx = 0;
        try (Stream<Path> paths = Files.walk(Paths.get("binaries"))) {
            for (Iterator<Path> it = paths.iterator(); it.hasNext();) {
                Path path = it.next();
                if (!Files.isRegularFile(path)) continue;

                byte[] data = Files.readAllBytes(path);
                fillPlacementOffset(data, placementOffset, fileIdx);
                fileIdx++;
            }
        }
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

    private record Placement(int ownerShndx, int fileIdx, String symbol) {}

}
