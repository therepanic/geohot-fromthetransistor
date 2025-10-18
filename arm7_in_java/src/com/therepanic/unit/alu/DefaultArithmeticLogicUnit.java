package com.therepanic.unit.alu;

import com.therepanic.registerFile.Mode;
import com.therepanic.registerFile.RegisterFile;
import com.therepanic.unit.Unit;
import com.therepanic.unit.UnitCondition;

public class DefaultArithmeticLogicUnit extends Unit implements ArithmeticLogicUnit {

    public DefaultArithmeticLogicUnit(RegisterFile registerFile) {
        super(registerFile);
    }

    @Override
    public void execute(UnitCondition condition, DataProcessingOperation operation, int rn, int rd, int operand2, boolean immediate, boolean s) {
        if (!checkCondition(condition)) return;
        int op1 = (operation == DataProcessingOperation.MOV) ? 0 : this.registerFile.read(rn);
        int op2 = immediate ? operand2 : this.registerFile.read(operand2);
        boolean oldC = this.registerFile.getFlagC();
        int result;
        boolean setC = oldC;
        boolean setV = this.registerFile.getFlagV();
        boolean shouldRestoreFromSPSR = shouldRestoreFromSPSR(rd, s);
        switch (operation) {
            case ADD -> {
                long sum = (op1 & 0xFFFFFFFFL) + (op2 & 0xFFFFFFFFL);
                result = (int) sum;
                setC = (sum >>> 32) != 0;
                setV = (((~(op1 ^ op2)) & (op1 ^ result)) & 0x80000000) != 0;
                this.registerFile.write(rd, result, shouldRestoreFromSPSR);
            }
            case SUB -> {
                long a = op1 & 0xFFFFFFFFL;
                long b = op2 & 0xFFFFFFFFL;
                result = (int) (a - b);
                setC = (a >= b);
                setV = (((op1 ^ op2) & (op1 ^ result)) & 0x80000000) != 0;
                this.registerFile.write(rd, result, shouldRestoreFromSPSR);
            }
            case AND -> {
                result = op1 & op2;
                this.registerFile.write(rd, result, shouldRestoreFromSPSR);
            }
            case CMP -> {
                long a = op1 & 0xFFFFFFFFL;
                long b = op2 & 0xFFFFFFFFL;
                int diff = (int) (a - b);
                setC = (a >= b);
                setV = (((op1 ^ op2) & (op1 ^ diff)) & 0x80000000) != 0;
                result = diff;
            }
            case MOV -> {
                result = op2;
                setV = this.registerFile.getFlagV();
                this.registerFile.write(rd, result, shouldRestoreFromSPSR);
            }
            default -> throw new RuntimeException("Unknown operation: " + operation);
        }
        if (s || operation == DataProcessingOperation.CMP) {
            this.registerFile.setFlagN((result & 0x80000000) != 0);
            this.registerFile.setFlagZ(result == 0);
            this.registerFile.setFlagC(setC);
            this.registerFile.setFlagV(setV);
        }
    }

    private boolean shouldRestoreFromSPSR(int rd, boolean s) {
        if (rd != 15 || !s) {
            return false;
        }
        Mode currentMode = this.registerFile.getCurrentMode();
        return !currentMode.equals(Mode.USER) && !currentMode.equals(Mode.SYS);
    }

}
