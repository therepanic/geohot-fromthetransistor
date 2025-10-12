package com.therepanic.parser.arm7;

import com.therepanic.Instruction;

import java.util.List;
import java.util.Map;
import java.util.Set;

public class ARM7MulMlaParser extends ARM7Parser {

    private static final Map<String, Integer> COND_CODES = Map.ofEntries(
            Map.entry("EQ", 0b0000),
            Map.entry("NE", 0b0001),
            Map.entry("CS", 0b0010),
            Map.entry("CC", 0b0011),
            Map.entry("MI", 0b0100),
            Map.entry("PL", 0b0101),
            Map.entry("VS", 0b0110),
            Map.entry("VC", 0b0111),
            Map.entry("HI", 0b1000),
            Map.entry("LS", 0b1001),
            Map.entry("GE", 0b1010),
            Map.entry("LT", 0b1011),
            Map.entry("GT", 0b1100),
            Map.entry("LE", 0b1101)
    );

    @Override
    public Instruction parse(List<String> tokens) {
        String op = tokens.getFirst().toUpperCase();
        int condCode = op.length() > 3 ? COND_CODES.getOrDefault(op.substring(3), 0b1110) : 0b1110;
        int instruction = 0;
        instruction |= condCode << 28;
        instruction |= 0b000000 << 22;
        instruction |= (op.startsWith("MLA") ? (1 << 21) : 0);
        instruction |= 0 << 20;
        int rd = parseReg(tokens.get(1));
        int rm = parseReg(tokens.get(2));
        int rs = parseReg(tokens.get(3));
        int rn = op.startsWith("MLA") ? parseReg(tokens.get(4)) : 0;
        instruction |= rd << 16;
        instruction |= rn << 12;
        instruction |= rs << 8;
        instruction |= 0b1001 << 4;
        instruction |= rm;
        return new Instruction(instruction);
    }

    private int parseReg(String token) {
        token = token.toUpperCase().trim();
        switch (token) {
            case "SP" -> {
                return 13;
            }
            case "LR" -> {
                return 14;
            }
            case "PC" -> {
                return 15;
            }
        }
        String digits = token.replaceAll("[^0-9]", "");
        return Integer.parseInt(digits);
    }

    @Override
    public Set<String> supportedOpcodes() {
        return Set.of("MUL", "MLA");
    }

}
