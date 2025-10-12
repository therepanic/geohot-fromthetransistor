package com.therepanic.parser.arm7;

import com.therepanic.Instruction;

import java.util.*;

public class ARM7SingleDataTransferParser extends ARM7Parser {

    @Override
    public Instruction parse(List<String> tokens) {
        String op = tokens.get(0).toUpperCase();

        boolean isByte = op.startsWith("STRB") || op.startsWith("LDRB");
        boolean isLoad = op.startsWith("LDR");

        int condCode;
        if (isByte) {
            condCode = (op.length() > 4) ? getConditionCodeOrDefault(op.substring(4), 0b1110) : 0b1110;
        } else {
            condCode = (op.length() > 3) ? getConditionCodeOrDefault(op.substring(3), 0b1110) : 0b1110;
        }

        int rd = parseReg(tokens.get(1).replace(",", "").trim());

        String rest = "";
        if (tokens.size() > 2) {
            rest = String.join(" ", tokens.subList(2, tokens.size())).trim();
        }

        int idxOpen = rest.indexOf('[');
        int idxClose = rest.indexOf(']');
        if (idxOpen < 0 || idxClose < 0 || idxClose < idxOpen) {
            throw new IllegalArgumentException("Invalid addressing: " + rest);
        }

        String insideRaw = rest.substring(idxOpen + 1, idxClose).trim();
        String outsideRaw = rest.substring(idxClose + 1).trim();

        boolean hasTrailingBang = outsideRaw.contains("!");
        String outsideNorm = outsideRaw.replace("!", "").replace(",", " ").trim();
        boolean isPostIndexed = !outsideNorm.isEmpty();
        String insideNorm = insideRaw.replace(",", " ").trim();

        String[] inParts = insideNorm.isEmpty() ? new String[0] : insideNorm.split("\\s+");
        if (inParts.length == 0) {
            throw new IllegalArgumentException("Missing base register: " + rest);
        }
        String rnStr = inParts[0];
        int rn = parseReg(rnStr);

        boolean preHasOffset = inParts.length > 1;

        int I = 0;
        int P = isPostIndexed ? 0 : 1;
        int U = 1;
        int B = isByte ? 1 : 0;
        int W;
        int L = isLoad ? 1 : 0;
        int offset12 = 0;

        if (isPostIndexed) {
            W = 0;
            if (!outsideNorm.isEmpty()) {
                OffsetEnc enc = parseOffset(outsideNorm);
                I = enc.I;
                U = enc.U;
                offset12 = enc.offset12;
            }
        } else {
            W = hasTrailingBang ? 1 : 0;
            if (preHasOffset) {
                String offExpr = String.join(" ", Arrays.copyOfRange(inParts, 1, inParts.length)).trim();
                OffsetEnc enc = parseOffset(offExpr);
                I = enc.I;
                U = enc.U;
                offset12 = enc.offset12;
            } else {
                I = 0;
                offset12 = 0;
            }
        }
        int instruction = (condCode << 28)
                | (1 << 26)
                | (I << 25)
                | (P << 24)
                | (U << 23)
                | (B << 22)
                | (W << 21)
                | (L << 20)
                | ((rn & 0xF) << 16)
                | ((rd & 0xF) << 12)
                | (offset12 & 0xFFF);
        return new Instruction(instruction);
    }

    @Override
    public Set<String> supportedOpcodes() {
        return Set.of("LDR", "STR", "LDRB", "STRB");
    }

    private static final class OffsetEnc {
        int I;
        int U;
        int offset12;
    }

    private OffsetEnc parseOffset(String expr) {
        OffsetEnc res = new OffsetEnc();
        String norm = expr.trim().replace(",", " ");
        String[] parts = norm.isEmpty() ? new String[0] : norm.split("\\s+");
        if (parts.length == 0) {
            res.I = 0;
            res.U = 1;
            res.offset12 = 0;
            return res;
        }

        String first = parts[0];

        if (first.startsWith("#")) {
            int imm = parseNumber(first.substring(1));
            res.I = 0;
            res.U = imm >= 0 ? 1 : 0;
            res.offset12 = encodeImmediate(Math.abs(imm));
            return res;
        }

        boolean negative = false;
        if (first.startsWith("-")) {
            negative = true;
            first = first.substring(1);
        } else if (first.startsWith("+")) {
            first = first.substring(1);
        }

        int rm = parseReg(first);
        int shiftType = 0;
        int shiftImm = 0;

        if (parts.length > 1) {
            String sType = parts[1].toUpperCase();
            if (isShift(sType)) {
                shiftType = shiftCode(sType);
                if (parts.length > 2) {
                    String sAmt = parts[2].trim();
                    if (sAmt.startsWith("#")) sAmt = sAmt.substring(1);
                    shiftImm = parseNumber(sAmt);
                    if (shiftImm < 0 || shiftImm > 31) {
                        throw new IllegalArgumentException("Shift amount out of range: " + shiftImm);
                    }
                }
            }
        }
        res.I = 1;
        res.U = negative ? 0 : 1;
        res.offset12 = ((shiftImm & 0x1F) << 7) | ((shiftType & 0x3) << 5) | (rm & 0xF);
        return res;
    }

    private int encodeImmediate(int value) {
        if (value < 0 || value > 0xFFF) throw new IllegalArgumentException("Immediate out of range: " + value);
        return value;
    }

    private int parseNumber(String s) {
        s = s.trim();
        boolean neg = false;
        if (s.startsWith("-")) {
            neg = true;
            s = s.substring(1);
        } else if (s.startsWith("+")) {
            s = s.substring(1);
        }
        int val;
        if (s.startsWith("0x") || s.startsWith("0X")) {
            val = Integer.parseInt(s.substring(2), 16);
        } else {
            val = Integer.parseInt(s, 10);
        }
        return neg ? -val : val;
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