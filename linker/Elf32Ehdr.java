public record Elf32Ehdr(
        int eType,
        int eMachine,
        long eVersion,
        long eEntry,
        long ePhoff,
        long eShoff,
        long eFlags,
        int eEhsize,
        int ePhentsize,
        int ePhnum,
        int eShentsize,
        int eShnum,
        int eShstrndx
) {}