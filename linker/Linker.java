import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Iterator;
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

    private static void link(byte[] data) {
        Elf32Ehdr header = parseHeader(data);
        Elf32Shdr[] sections = parseSections(data, header);
    }

    public static void main(String[] args) throws IOException {
        try (Stream<Path> paths = Files.walk(Paths.get("binaries"))) {
            for (Iterator<Path> it = paths.iterator(); it.hasNext();) {
                Path path = it.next();
                if (!Files.isRegularFile(path)) continue;

                byte[] data = Files.readAllBytes(path);
                link(data);
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

}
