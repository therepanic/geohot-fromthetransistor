package com.therepanic.parser.arm7;

import com.therepanic.Instruction;

import java.util.List;
import java.util.Map;
import java.util.Set;

public class ARM7BranchParser  extends ARM7Parser {

    private Map<String, Integer> labelAddressMap;

    private Integer curAddress;

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

    public ARM7BranchParser() {
    }

    public ARM7BranchParser(Map<String, Integer> labelAddressMap) {
        this.labelAddressMap = labelAddressMap;
    }

    @Override
    public Instruction parse(List<String> tokens) {
        String op = tokens.getFirst();
        int condCode = 0;
        boolean linked = false;
        if (op.startsWith("BL")) {
            linked = true;
            if (op.length() == 2) {
                condCode = 0b1110;
            } else {
                String cond = op.substring(2);
                condCode = COND_CODES.get(cond);
            }
        } else if (op.startsWith("B")) {
            if (op.length() == 1) {
                condCode = 0b1110;
            } else {
                String cond = op.substring(1);
                condCode = COND_CODES.get(cond);
            }
        }
        int labelAddress = this.labelAddressMap.get(tokens.get(1));
        int instruction = 0;
        instruction |= condCode << 28;
        instruction |= 0b101 << 25;
        instruction |= (linked ? 1 << 24 : 0 << 24);
        instruction |= calculateBranchOffset(this.curAddress, labelAddress);
        return new Instruction(instruction);
    }

    @Override
    public Set<String> supportedOpcodes() {
        return Set.of("B", "BL");
    }

    private int calculateBranchOffset(int currentByteAddr, int targetByteAddr) {
        int pc = currentByteAddr + 8;
        int rawOffset = targetByteAddr - pc;
        int imm24 = rawOffset >> 2;
        return imm24 & 0x00FFFFFF;
    }

    public Map<String, Integer> getLabelAddressMap() {
        return this.labelAddressMap;
    }

    public void setLabelAddressMap(Map<String, Integer> labelAddressMap) {
        this.labelAddressMap = labelAddressMap;
    }

    public Integer getCurAddress() {
        return curAddress;
    }

    public void setCurAddress(Integer curAddress) {
        this.curAddress = curAddress;
    }

}
