import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

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

    static byte[] readSectionBytes(byte[] file, Elf32_Shdr sh) {
        if (sh.shType == SHT_NOBITS) {
            return new byte[(int) sh.shSize];
        }
        int off = (int) sh.shOffset;
        int size = (int) sh.shSize;
        byte[] out = new byte[size];
        System.arraycopy(file, off, out, 0, size);
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

    public static String flagsToString(long shFlags) {
        StringBuilder sb = new StringBuilder();
        if ((shFlags & SHF_ALLOC) != 0) sb.append('A');
        if ((shFlags & SHF_WRITE) != 0) sb.append('W');
        if ((shFlags & SHF_EXECINSTR) != 0) sb.append('X');
        return sb.toString();
    }

    static String typeToString(long shType) {
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

    public static void main(String[] args) throws IOException {
        Path path = Path.of("c.o");
        byte[] data = Files.readAllBytes(path);

        Elf32_Ehdr hdr = parseElf32Header(data);
        Elf32_Shdr[] shdrs = parseSectionHeaders(data, hdr);
        Elf32_Shdr shstrSh = shdrs[hdr.eShstrndx()];
        byte[] shstrtab = readSectionBytes(data, shstrSh);
        System.out.println("\n[Sections]");
        for (int i = 0; i < shdrs.length; i++) {
            Elf32_Shdr sh = shdrs[i];
            String secName = (sh.shName() < shstrtab.length) ? readCString(shstrtab, (int) sh.shName()) : "<bad>";
            System.out.printf(
                    "%2d: %-16s type=%-8s flags=%-3s off=0x%06x size=0x%06x link=%d info=%d entsz=%d%n",
                    i,
                    secName,
                    typeToString(sh.shType),
                    flagsToString(sh.shFlags),
                    sh.shOffset,
                    sh.shSize,
                    sh.shLink,
                    sh.shInfo,
                    sh.shEntsize
            );
        }
    }
}