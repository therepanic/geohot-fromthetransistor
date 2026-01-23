import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.stream.Stream;

public class Linker {

    // ELF constants (ELF32)
    private static final byte[] ELF_MAGIC = new byte[] { 0x7F, 'E', 'L', 'F' };
    private static final int ELFCLASS32 = 1;
    private static final int ELFDATA2LSB = 1;

    // Section header types
    private static final long SHT_NULL = 0;
    private static final long SHT_PROGBITS = 1;
    private static final long SHT_SYMTAB = 2;
    private static final long SHT_STRTAB = 3;
    private static final long SHT_RELA = 4;
    private static final long SHT_NOBITS = 8;
    private static final long SHT_REL = 9;
    private static final long SHT_DYNSYM = 11;

    // Section flags (subset)
    private static final long SHF_WRITE = 0x1;
    private static final long SHF_ALLOC = 0x2;
    private static final long SHF_EXECINSTR = 0x4;

    // ARM relocation types (subset)
    public static final int R_ARM_CALL = 28;
    public static final int R_ARM_JUMP24 = 29;
    public static final int R_ARM_ABS32 = 2;

    public static final Map<Integer, String> ARM_RELOC_NAMES = Map.of(
            R_ARM_ABS32, "R_ARM_ABS32",
            R_ARM_CALL,  "R_ARM_CALL",
            R_ARM_JUMP24, "R_ARM_JUMP24"
    );

    record Placement(int outSec, int outOff) {}

    record SectionKey(int fileIndex, int shndx) {}

    public record Elf32_Sym(
            long stName,
            long stValue,
            long stSize,
            int stInfo,
            int stOther,
            int stShndx
    ) {}

    public record Elf32_Rel(
            long rOffset,
            long rInfo
    ) {}

    public record Elf32_Ehdr(
            int eType,
            int eMachine,
            long eVersion,
            long eEntry,
            long ePhoff,
            long eShoff,
            long eFlags,
            int eEhsize,
            int ePhentSize,
            int ePhnum,
            int eShentsize,
            int eShnum,
            int eShstrndx
    ) {}

    public record Elf32_Shdr(
            long shName,
            long shType,
            long shFlags,
            long shAddr,
            long shOffset,
            long shSize,
            long shLink,
            long shInfo,
            long shAddralign,
            long shEntsize
    ) {}

    static String bindToString(int bind) {
        return switch (bind) {
            case 0 -> "LOCAL";
            case 1 -> "GLOBAL";
            case 2 -> "WEAK";
            default -> "BIND(" + bind + ")";
        };
    }

    static String symTypeToString(int type) {
        return switch (type) {
            case 0 -> "NOTYPE";
            case 1 -> "OBJECT";
            case 2 -> "FUNC";
            case 3 -> "SECTION";
            case 4 -> "FILE";
            default -> "TYPE(" + type + ")";
        };
    }

    public static int u16le(byte[] b, int off) {
        return (b[off] & 0xFF)
                | ((b[off + 1] & 0xFF) << 8);
    }

    public static long u32le(byte[] b, int off) {
        return (b[off] & 0xFF)
                | ((long) (b[off + 1] & 0xFF) << 8)
                | ((long) (b[off + 2] & 0xFF) << 16)
                | ((long) (b[off + 3] & 0xFF) << 24);
    }

    public static String readCString(byte[] buf, int start) {
        int i = start;
        while (i < buf.length && buf[i] != 0) i++;
        return new String(buf, start, i - start);
    }

    public static int read32le(byte[] b, int off) {
        return (b[off] & 0xFF) | ((b[off + 1] & 0xFF) << 8) | ((b[off + 2] & 0xFF) << 16) | ((b[off + 3] & 0xFF) << 24);
    }

    public static void write32le(byte[] b, int off, int v) {
        b[off] = (byte) (v);
        b[off + 1] = (byte) (v >>> 8);
        b[off + 2] = (byte) (v >>> 16);
        b[off + 3] = (byte) (v >>> 24);
    }

    public static byte[] readSectionBytes(byte[] file, Elf32_Shdr sh) {
        if (sh.shType() == SHT_NOBITS) {
            return new byte[(int) sh.shSize()];
        }
        int off = (int) sh.shOffset();
        int size = (int) sh.shSize();
        byte[] out = new byte[size];
        System.arraycopy(file, off, out, 0, size);
        return out;
    }

    public static Elf32_Sym[] parseSymtab(byte[] data, int shEntsize) {
        int n = data.length / shEntsize;
        Elf32_Sym[] out = new Elf32_Sym[n];
        for (int i = 0; i < n; i++) {
            int off = i * shEntsize;
            long stName = u32le(data, off);
            long stValue = u32le(data, off + 4);
            long stSize = u32le(data, off + 8);
            int stInfo = data[off + 12] & 0xFF;
            int stOther = data[off + 13] & 0xFF;
            int stShndx = u16le(data, off + 14);
            out[i] = new Elf32_Sym(stName, stValue, stSize, stInfo, stOther, stShndx);
        }
        return out;
    }

    public static int findSectionByType(Elf32_Shdr[] shdrs, long type) {
        for (int i = 0; i < shdrs.length; i++) {
            if (shdrs[i].shType() == type) return i;
        }
        return -1;
    }


    public static String secName(Elf32_Shdr sh, byte[] shstrtab) {
        return (sh.shName() < shstrtab.length) ? readCString(shstrtab, (int) sh.shName()) : "<bad>";
    }
    public static Elf32_Rel[] parseRel(byte[] data, int shEntsize) {
        int n = data.length / shEntsize;
        Elf32_Rel[] out = new Elf32_Rel[n];
        for (int i = 0; i < n; i++) {
            int off = i * shEntsize;
            long rOffset = u32le(data, off);
            long rInfo = u32le(data, off + 4);
            out[i] = new Elf32_Rel(rOffset, rInfo);
        }
        return out;
    }

    public static Elf32_Ehdr parseElf32Header(byte[] data) {
        if (data == null || data.length < 16) {
            throw new IllegalArgumentException("File too small to be an ELF header");
        }

        // magic
        for (int i = 0; i < 4; i++) {
            if (data[i] != ELF_MAGIC[i]) {
                throw new IllegalArgumentException("Not an ELF file");
            }
        }

        int eiClass = data[4] & 0xFF;
        int eiData  = data[5] & 0xFF;

        if (eiClass != ELFCLASS32) {
            throw new IllegalArgumentException("Not ELF32");
        }
        if (eiData != ELFDATA2LSB) {
            throw new IllegalArgumentException("Not little-endian");
        }
        int off = 16;
        int eType = u16le(data, off); off += 2;
        int eMachine = u16le(data, off); off += 2;
        long eVersion = u32le(data, off); off += 4;
        long eEntry = u32le(data, off); off += 4;
        long ePhoff = u32le(data, off); off += 4;
        long eShoff = u32le(data, off); off += 4;
        long eFlags = u32le(data, off); off += 4;
        int eEhsize = u16le(data, off); off += 2;
        int ePhentSize = u16le(data, off); off += 2;
        int ePhnum = u16le(data, off); off += 2;
        int eShentsize = u16le(data, off); off += 2;
        int eShnum = u16le(data, off); off += 2;
        int eShstrndx = u16le(data, off);
        return new Elf32_Ehdr(eType, eMachine, eVersion, eEntry, ePhoff, eShoff, eFlags, eEhsize, ePhentSize, ePhnum,
                eShentsize, eShnum, eShstrndx);
    }

    public static Elf32_Shdr[] parseSectionHeaders(byte[] data, Elf32_Ehdr eh) {
        Elf32_Shdr[] elf32Shdrs = new Elf32_Shdr[eh.eShnum];
        for (int i = 0; i < elf32Shdrs.length; i++) {
            int off = (int) (eh.eShoff + (long) i * eh.eShentsize);
            long shName = u32le(data, off);
            long shType = u32le(data, off + 4);
            long shFlags = u32le(data, off + 8);
            long shAddr = u32le(data, off + 12);
            long shOffset = u32le(data, off + 16);
            long shSize = u32le(data, off + 20);
            long shLink = u32le(data, off + 24);
            long shInfo = u32le(data, off + 28);
            long shAddralign = u32le(data, off + 32);
            long shEntsize = u32le(data, off + 36);
            elf32Shdrs[i] = new Elf32_Shdr(shName, shType, shFlags, shAddr, shOffset, shSize, shLink, shInfo, shAddralign, shEntsize);
        }
        return elf32Shdrs;
    }

    public static int alignUp(int x, int a) {
        int mask = a - 1;
        return (x + mask) & ~mask;
    }

    public static String flagsToString(long shFlags) {
        StringBuilder sb = new StringBuilder();
        if ((shFlags & SHF_ALLOC) != 0) sb.append('A');
        if ((shFlags & SHF_WRITE) != 0) sb.append('W');
        if ((shFlags & SHF_EXECINSTR) != 0) sb.append('X');
        return sb.toString();
    }

    public static String typeToString(long shType) {
        return switch ((int) shType) {
            case (int) SHT_NULL -> "NULL";
            case (int) SHT_PROGBITS -> "PROGBITS";
            case (int) SHT_SYMTAB -> "SYMTAB";
            case (int) SHT_STRTAB -> "STRTAB";
            case (int) SHT_RELA -> "RELA";
            case (int) SHT_REL -> "REL";
            case (int) SHT_NOBITS -> "NOBITS";
            case (int) SHT_DYNSYM -> "DYNSYM";
            default -> "TYPE(" + shType + ")";
        };
    }

    public static int ELF32_R_SYM(long rInfo)  { return (int) (rInfo >>> 8); }

    public static int ELF32_R_TYPE(long rInfo) {
        return (int) (rInfo & 0xFF);
    }

    public static int ELF32_ST_BIND(int stInfo) {
        return (stInfo >>> 4) & 0xF;
    }

    public static int ELF32_ST_TYPE(int stInfo) {
        return stInfo & 0xF;
    }

    public static long countSums(Path path) throws IOException {
        long cursor = 0;
        byte[] data = Files.readAllBytes(path);
        Elf32_Ehdr hdr = parseElf32Header(data);
        Elf32_Shdr[] shdrs = parseSectionHeaders(data, hdr);
        for (int i = 0; i < shdrs.length; i++) {
            Elf32_Shdr sh = shdrs[i];
            boolean isExecAlloc = sh.shType() == SHT_PROGBITS && (sh.shFlags() & SHF_ALLOC) != 0 && (sh.shFlags() & SHF_EXECINSTR) != 0;
            if (isExecAlloc) {
                int align = (int) Math.max(1, sh.shAddralign());
                cursor = alignUp((int) cursor, align);
                cursor += sh.shSize();
            }
        }
        return cursor;
    }


    public static void link(Path path, Map<SectionKey, Placement> placementMap, int fileIdx, byte[] outText, int[] cursor) throws IOException {
        byte[] data = Files.readAllBytes(path);

        Elf32_Ehdr hdr = parseElf32Header(data);
        Elf32_Shdr[] shdrs = parseSectionHeaders(data, hdr);
        Elf32_Shdr shstrSh = shdrs[hdr.eShstrndx()];
        byte[] shstrtab = readSectionBytes(data, shstrSh);

        byte[] textSum = null;

        for (int i = 0; i < shdrs.length; i++) {
            Elf32_Shdr sh = shdrs[i];
            boolean isExecAlloc =
                    sh.shType() == SHT_PROGBITS &&
                            (sh.shFlags() & SHF_ALLOC) != 0 &&
                            (sh.shFlags() & SHF_EXECINSTR) != 0;

            if (isExecAlloc) {
                byte[] bytes = readSectionBytes(data, sh);
                int align = (int) Math.max(1, sh.shAddralign());
                cursor[0] = alignUp(cursor[0], align);
                System.arraycopy(bytes, 0, outText, cursor[0], bytes.length);
                placementMap.put(new SectionKey(fileIdx, i), new Placement(/*OUT_TEXT*/1, cursor[0]));
                cursor[0] += bytes.length;
            }
        }

        for (int i = 0; i < shdrs.length; i++) {
            Elf32_Shdr sh = shdrs[i];
            String secName = secName(sh, shstrtab);
            if (sh.shType() == SHT_PROGBITS && (sh.shFlags() & SHF_ALLOC) != 0 || sh.shType() == SHT_NOBITS) {

            }
        }

        // symtab
        System.out.println(Arrays.toString(shdrs));
        int symtabIdx = findSectionByType(shdrs, SHT_SYMTAB);
        Elf32_Sym[] syms = null;
        byte[] strtab = null;
        if (symtabIdx != -1) {
            Elf32_Shdr symSh = shdrs[symtabIdx];
            byte[] symBytes = readSectionBytes(data, symSh);
            int strtabIdx = (int) symSh.shLink();
            syms = parseSymtab(symBytes, (int) symSh.shEntsize());
            strtab = readSectionBytes(data, shdrs[strtabIdx]);
            for (int i = 0; i < syms.length; i++) {
                Elf32_Sym s = syms[i];
                String sname = (s.stName() < strtab.length) ? readCString(strtab, (int) s.stName()) : "<bad>";
                int bind = ELF32_ST_BIND(s.stInfo());
                int type = ELF32_ST_TYPE(s.stInfo());
                System.out.printf(
                        "sym[%03d] %-24s %-6s %-6s shndx=%-4d value=0x%08x size=%d%n",
                        i,
                        sname,
                        bindToString(bind),
                        symTypeToString(type),
                        s.stShndx(),
                        (int) s.stValue(),
                        (int) s.stSize()
                );
            }
        } else {
            System.out.println("symtab not found");
        }
        for (int i = 0; i < shdrs.length; i++) {
            Elf32_Shdr shr = shdrs[i];
            if (shr.shType() != SHT_REL) continue;
            String relName = secName(shr, shstrtab);
            int targetSecIdx = (int) shr.shInfo(); // reloc section
            String targetName = (targetSecIdx >= 0 && targetSecIdx < shdrs.length) ? secName(shdrs[targetSecIdx], shstrtab) : "<bad-target>";

            byte[] relBytes = readSectionBytes(data, shr);
            Elf32_Rel[] rels = parseRel(relBytes, (int) shr.shEntsize());

            System.out.printf("-- %s applies to %s (entries=%d) --%n", relName, targetName, rels.length);

            for (int j = 0; j < rels.length; j++) {
                Elf32_Rel r = rels[j];
                int rType = ELF32_R_TYPE(r.rInfo());
                int rSym  = ELF32_R_SYM(r.rInfo());
                String rTypeName = ARM_RELOC_NAMES.getOrDefault(rType, "R_ARM_(" + rType + ")");

                String symName =  "empty";
                if (syms != null && strtab != null && rSym >= 0 && rSym < syms.length) {
                    long stName = syms[rSym].stName();
                    symName = (stName < strtab.length) ? readCString(strtab, (int) stName) : "<bad>";
                }

                if (rType == R_ARM_ABS32) {
                    //1 - .text for now
                    Placement placement = placementMap.get(new SectionKey(fileIdx, (int) r.rOffset()));
                }

                System.out.printf(
                        "rel[%03d] off=0x%08x type=%-12s sym[%d]=%s%n",
                        j,
                        (int) r.rOffset(),
                        rTypeName,
                        rSym,
                        symName
                );
            }
        }
    }

    public static void main(String[] args) throws IOException {
        Map<SectionKey, Placement> placement = new HashMap<>();
        int idx = 0;
        long outputTextSize = 0;
        try (Stream<Path> paths = Files.walk(Paths.get(""))) {
            for (Iterator<Path> it = paths.iterator(); it.hasNext();) {
                Path path = it.next();

                outputTextSize += countSums(path);
            }
        }
        int[] cursor = new int[1];
        byte[] outputText = new byte[(int) outputTextSize];
        try (Stream<Path> paths = Files.walk(Paths.get(""))) {
            for (Iterator<Path> it = paths.iterator(); it.hasNext();) {
                Path path = it.next();

                link(path, placement, idx, outputText, cursor);
                idx++;
            }
        }
    }
}