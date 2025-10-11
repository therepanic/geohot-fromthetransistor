package com.therepanic.parser.arm7;

import com.therepanic.Instruction;

import java.util.List;
import java.util.Map;
import java.util.Set;

public class ARM7SoftwareInterruptParser extends ARM7Parser {

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
        System.out.println(tokens);
        String op = tokens.getFirst().toUpperCase();
        int condCode = op.length() > 3 ? COND_CODES.getOrDefault(op.substring(3), 0b1110) : 0b1110;
        int instruction = 0;
        instruction |= condCode << 28;
        instruction |= 0b1111 << 24;
        int comment;
        if (tokens.get(1).startsWith("0X")) {
            comment = Integer.parseInt(tokens.get(1).substring(2), 16);
        } else {
            comment = Integer.parseInt(tokens.get(1));
        }
        instruction |= comment & 0xFFFFFF;
        return new Instruction(instruction);
    }

    @Override
    public Set<String> supportedOpcodes() {
        return Set.of("SWI");
    }

}
