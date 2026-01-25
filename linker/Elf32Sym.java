public record Elf32Sym(
        long stName,
        long stValue,
        long stSize,
        long stInfo,
        long stOther,
        int stShndx
) {
}
