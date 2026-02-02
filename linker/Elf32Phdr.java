public record Elf32Phdr(
        long pType,
        long pOffset,
        long pvAddr,
        long ppAddr,
        long pFileSz,
        long pMemSz,
        long pFlags,
        long pAlign
) {
}
