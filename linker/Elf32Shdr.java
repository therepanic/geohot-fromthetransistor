public record Elf32Shdr(
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
