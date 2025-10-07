package com.therepanic.parser.arm7;

import com.therepanic.Instruction;

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

    public ARM7DataProcessingParser() {
    }

    @Override
    public Instruction parse(List<String> tokens) {
        String op = tokens.getFirst();
        int opCode = OP_CODES.get(op.substring(0, 3));
        int condCode;
        if (op.length() > 3) {
            condCode = COND_CODES.get(op.substring(3));
        } else {
            //AL cond code
            condCode = 0b1110;
        }
        for (var opCodeEntry : OP_CODES.entrySet()) {
            if (op.startsWith(opCodeEntry.getKey())) {
                opCode = opCodeEntry.getValue();
                if (op.length() > 3) {
                    condCode = COND_CODES.get(op.substring(3));
                }
                break;
            }
        }
        int instruction = 0;
        instruction |= condCode << 28;
        instruction |= opCode << 21;
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
                    String numStr = s.substring(1);
                    if (numStr.startsWith("0x")) {
                        op2 = Integer.parseInt(numStr.substring(2), 16);
                    } else {
                        op2 = Integer.parseInt(numStr, 10);
                    }
                    op2 = encodeImmediate(op2);
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
                    String numStr = s.substring(1);
                    if (numStr.startsWith("0x")) {
                        op2 = Integer.parseInt(numStr.substring(2), 16);
                    } else {
                        op2 = Integer.parseInt(numStr, 10);
                    }
                    op2 = encodeImmediate(op2);
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
                    String numStr = s.substring(1);
                    if (numStr.startsWith("0x")) {
                        op2 = Integer.parseInt(numStr.substring(2), 16);
                    } else {
                        op2 = Integer.parseInt(numStr, 10);
                    }
                    op2 = encodeImmediate(op2);
                    imm = true;
                } else {
                    int rm = parseReg(s);
                    //optional shift
                    if (tokens.size() > 5 && isShift(tokens.get(4))) {
                        int sc = shiftCode(tokens.get(4));
                        int sh;
                        if (tokens.get(5).startsWith("#")) {
                            //shift by immediate
                            String numStr = tokens.get(5).replace("#", "");
                            if (numStr.startsWith("0x")) {
                                sh = Integer.parseInt(numStr.substring(2), 16) & 0x1F;
                            } else {
                                sh = Integer.parseInt(numStr, 10) & 0x1F;
                            }
                            op2 = (sh << 7) | (sc << 5) | (rm & 0xF);
                        } else if ("RRX".equals(tokens.get(5))) {
                            //edge case
                            op2 = (0b11 << 5) | (rm & 0xF);
                        } else {
                            //shift by reg
                            int rs = parseReg(tokens.get(5));
                            op2 = (rs << 8) | (sc << 5) | (1 << 4) | (rm & 0xF);
                        }
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

    private int encodeImmediate(int value) {
        for (int rotate = 0; rotate < 32; rotate += 2) {
            int rotated = Integer.rotateRight(value, rotate);
            if ((rotated & 0xFFFFFF00) == 0) {
                int imm8 = rotated & 0xFF;
                int rotateImm = rotate / 2;
                return (rotateImm << 8) | imm8;
            }
        }
        throw new IllegalArgumentException("Not correct immediate");
    }

    private int parseReg(String token) {
        if (token.equals("SP")) {
            return 13;
        }
        if (token.equals("LR")) {
            return 14;
        }
        if (token.equals("PC")) {
            return 15;
        }
        String digits = token.replaceAll("[^0-9]", "");
        return Integer.parseInt(digits);
    }

    private boolean isShift(String s) {
        return "LSL".equals(s) || "LSR".equals(s) || "ASR".equals(s) || "ROR".equals(s);
    }

    private int shiftCode(String s) {
        return switch (s) {
            case "LSL" -> 0b00;
            case "LSR" -> 0b01;
            case "ASR" -> 0b10;
            default -> 0b11; // ROR
        };
    }

}
