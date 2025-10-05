package com.therepanic.parser.arm7;

import com.therepanic.Instruction;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

public class ARM7DataProcessingParser extends ARM7Parser {

    private static final Map<String, Integer> OP_CODES = Map.ofEntries(
            Map.entry("AND", 0b0000),
            Map.entry("EOR", 0b0001),
            Map.entry("SUB", 0b0010),
            Map.entry("RSB", 0b0011),
            Map.entry("ADD", 0b0100),
            Map.entry("ADC", 0b0101),
            Map.entry("SBC", 0b0110),
            Map.entry("RSC", 0b0111),
            Map.entry("TST", 0b1000),
            Map.entry("TEQ", 0b1001),
            Map.entry("CMP", 0b1010),
            Map.entry("CMN", 0b1011),
            Map.entry("ORR", 0b1100),
            Map.entry("MOV", 0b1101),
            Map.entry("BIC", 0b1110),
            Map.entry("MVN", 0b1111)
    );

    @Override
    public Instruction parse(String[] entry) {
        List<String> tokens = Arrays.stream(entry)
                .map(t -> t.replace(",", "").trim())
                .filter(t -> !t.isEmpty())
                .map(String::toUpperCase)
                .toList();
        String op = tokens.getFirst();
        int instruction = 0;
        instruction |= 0b1110 << 28;
        instruction |= (OP_CODES.get(op) & 0b1111) << 21;
        if ("CMP".equals(op) || "CMN".equals(op) || "TST".equals(op) || "TEQ".equals(op)) {
            instruction |= 1 << 20;
        } else {
            instruction |= 0 << 20;
        }
        int rd = 0, rn = 0, op2 = 0;
        boolean imm = false;
        if ("MOV".equals(op) || "MVN".equals(op)) {
            if (tokens.size() > 1) {
                rd = parseReg(tokens.get(1));
            }
            if (tokens.size() > 2) {
                String s = tokens.get(2);
                if (s.startsWith("#")) {
                    op2 = Integer.parseInt(s.substring(1)) & 0xFF;
                    imm = true;
                } else {
                    op2 = parseReg(s);
                }
            }
        } else if ("CMP".equals(op) || "CMN".equals(op) || "TST".equals(op) || "TEQ".equals(op)) {
            if (tokens.size() > 1) {
                rn = parseReg(tokens.get(1));
            }
            if (tokens.size() > 2) {
                String s = tokens.get(2);
                if (s.startsWith("#")) {
                    op2 = Integer.parseInt(s.substring(1)) & 0xFF;
                    imm = true;
                } else {
                    op2 = parseReg(s);
                }
            }
        } else {
            if (tokens.size() > 1) {
                rd = parseReg(tokens.get(1));
            }
            if (tokens.size() > 2) {
                rn = parseReg(tokens.get(2));
            }
            if (tokens.size() > 3) {
                String s = tokens.get(3);
                if (s.startsWith("#")) {
                    op2 = Integer.parseInt(s.substring(1)) & 0xFF;
                    imm = true;
                } else {
                    int rm = parseReg(s);
                    //optional shift
                    if (tokens.size() > 5 && isShift(tokens.get(4))) {
                        int sc = shiftCode(tokens.get(4));
                        int sh = Integer.parseInt(tokens.get(5).replace("#", "")) & 0x1F;
                        op2 = (sh << 7) | (sc << 5) | (rm & 0xF);
                    } else {
                        op2 = rm & 0xF;
                    }
                }
            }
        }
        instruction |= (imm ? 0b001 : 0b000) << 25;
        instruction |= (rn & 0xF) << 16;
        instruction |= (rd & 0xF) << 12;
        instruction |= op2 & 0xFFF;
        return new Instruction(instruction);
    }

    private int parseReg(String token) {
        return Integer.parseInt(token.replaceAll("[^0-9]", ""));
    }

    private boolean isShift(String s) { return "LSL".equals(s) || "LSR".equals(s) || "ASR".equals(s) || "ROR".equals(s); }

    private int shiftCode(String s) {
        return switch (s) {
            case "LSL" -> 0b00;
            case "LSR" -> 0b01;
            case "ASR" -> 0b10;
            default -> 0b11; // ROR
        };
    }

}
