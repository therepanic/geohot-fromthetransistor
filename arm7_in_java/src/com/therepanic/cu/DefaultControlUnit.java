package com.therepanic.cu;

import com.therepanic.dataMemory.SRamDataMemory;
import com.therepanic.instructionMemory.InstructionMemory;
import com.therepanic.registerFile.RegisterFile;
import com.therepanic.unit.UnitCondition;
import com.therepanic.unit.alu.ArithmeticLogicUnit;
import com.therepanic.unit.alu.DataProcessingOperation;
import com.therepanic.unit.alu.DefaultArithmeticLogicUnit;
import com.therepanic.unit.branch.BranchLogicUnit;
import com.therepanic.unit.branch.DefaultBranchLogicUnit;
import com.therepanic.unit.sdt.DefaultSingleDataTransferUnit;
import com.therepanic.unit.sdt.SingleDataTransferUnit;

import java.util.Map;

public class DefaultControlUnit implements ControlUnit {

    private static final Map<Integer, String> COND_CODES = Map.ofEntries(
            Map.entry(0b0000, "EQ"),
            Map.entry(0b0001, "NE"),
            Map.entry(0b0010, "CS"),
            Map.entry(0b0011, "CC"),
            Map.entry(0b0100, "MI"),
            Map.entry(0b0101, "PL"),
            Map.entry(0b0110, "VS"),
            Map.entry(0b0111, "VC"),
            Map.entry(0b1000, "HI"),
            Map.entry(0b1001, "LS"),
            Map.entry(0b1010, "GE"),
            Map.entry(0b1011, "LT"),
            Map.entry(0b1100, "GT"),
            Map.entry(0b1101, "LE"),
            Map.entry(0b1110, "AL")
    );

    private static final Map<Integer, String> OP_CODES = Map.ofEntries(
            Map.entry(0b0000, "AND"),
            Map.entry(0b0100, "ADD"),
            Map.entry(0b0010, "SUB"),
            Map.entry(0b1010, "CMP"),
            Map.entry(0b1101, "MOV")
    );

    private final InstructionMemory instructionMemory;

    private final RegisterFile registerFile;

    private final ArithmeticLogicUnit alu;

    private final BranchLogicUnit blu;

    private final SingleDataTransferUnit sdt;

    public DefaultControlUnit(InstructionMemory instructionMemory, RegisterFile registerFile, SRamDataMemory dataMemory) {
        this.instructionMemory = instructionMemory;
        this.registerFile = registerFile;
        this.alu = new DefaultArithmeticLogicUnit(registerFile);
        this.blu = new DefaultBranchLogicUnit(registerFile);
        this.sdt = new DefaultSingleDataTransferUnit(registerFile, dataMemory);
    }

    @Override
    public int fetch() {
        int pc = this.registerFile.read(15);
        int instr = this.instructionMemory.readInstruction(pc);
        this.registerFile.write(15, pc + 4, false);
        return instr;
    }

    @Override
    public void decodeAndExecute(int instruction) {
        int typeBits = (instruction >>> 25) & 0b111;
        switch (typeBits) {
            case 0b000, 0b001 -> {
                //data processing
                int condBit = (instruction >>> 28) & 0b1111;
                int iBit = (instruction >>> 25) & 0b1;
                int opCodeBit = (instruction >>> 21) & 0b1111;
                int sBit = (instruction >>> 20) & 0b1;
                int rnBit = (instruction >>> 16) & 0b1111;
                int rdBit = (instruction >>> 12) & 0b1111;
                int op2Bit = (instruction) & 0xFFF;

                this.alu.execute(
                        UnitCondition.valueOf(COND_CODES.get(condBit)),
                        DataProcessingOperation.valueOf(OP_CODES.get(opCodeBit)),
                        rnBit,
                        rdBit,
                        op2Bit,
                        iBit == 1,
                        sBit == 1
                );
            }
            case 0b010, 0b011 -> {
                //single data transfer
                int condBit = (instruction >>> 28) & 0b1111;
                int iBit = (instruction >>> 25) & 0b1;
                int pBit = (instruction >>> 24) & 0b1;
                int uBit = (instruction >>> 23) & 0b1;
                int bBit = (instruction >>> 22) & 0b1;
                int wBit = (instruction >>> 21) & 0b1;
                int lBit = (instruction >>> 20) & 0b1;
                int rnBit = (instruction >>> 16) & 0b1111;
                int rdBit = (instruction >>> 12) & 0b1111;
                int op2Bit = (instruction) & 0xFFF;

                this.sdt.execute(
                        UnitCondition.valueOf(COND_CODES.get(condBit)),
                        iBit == 1,
                        pBit == 1,
                        uBit == 1,
                        bBit != 1,
                        wBit == 1,
                        lBit == 1,
                        rnBit,
                        rdBit,
                        op2Bit
                );
            }
            case 0b101 -> {
                //branch
                int condBit = (instruction >>> 28) & 0b1111;
                int lBit = (instruction >>> 24) & 0b1;
                int offset = instruction & 0xFFFFFF;
                this.blu.execute(
                        UnitCondition.valueOf(COND_CODES.get(condBit)),
                        lBit == 1,
                        offset
                );
            }
        }
    }

}
