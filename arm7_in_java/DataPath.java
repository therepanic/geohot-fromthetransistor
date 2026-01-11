package com.therepanic;

import com.therepanic.cu.ControlUnit;
import com.therepanic.cu.DefaultControlUnit;
import com.therepanic.dataMemory.DefaultSRamDataMemory;
import com.therepanic.dataMemory.SRamDataMemory;
import com.therepanic.instructionMemory.DefaultInstructionMemory;
import com.therepanic.instructionMemory.InstructionMemory;
import com.therepanic.registerFile.DefaultRegisterFile;
import com.therepanic.registerFile.RegisterFile;
import com.therepanic.unit.alu.ArithmeticLogicUnit;
import com.therepanic.unit.alu.DefaultArithmeticLogicUnit;
import com.therepanic.unit.branch.BranchLogicUnit;
import com.therepanic.unit.branch.DefaultBranchLogicUnit;
import com.therepanic.unit.sdt.DefaultSingleDataTransferUnit;
import com.therepanic.unit.sdt.SingleDataTransferUnit;

public class DataPath {

    private final ControlUnit controlUnit;

    private final SRamDataMemory dataMemory;

    private final InstructionMemory instructionMemory;

    private final RegisterFile registerFile;

    private final ArithmeticLogicUnit alu;

    private final BranchLogicUnit blu;

    private final SingleDataTransferUnit sdtu;

    public DataPath(ControlUnit controlUnit, SRamDataMemory dataMemory, InstructionMemory instructionMemory, RegisterFile registerFile, ArithmeticLogicUnit alu, BranchLogicUnit blu, SingleDataTransferUnit sdtu) {
        this.controlUnit = controlUnit;
        this.dataMemory = dataMemory;
        this.instructionMemory = instructionMemory;
        this.registerFile = registerFile;
        this.alu = alu;
        this.blu = blu;
        this.sdtu = sdtu;
    }

    public DataPath() {
        this.dataMemory = new DefaultSRamDataMemory();
        this.registerFile = new DefaultRegisterFile();
        this.instructionMemory = new DefaultInstructionMemory();
        this.controlUnit = new DefaultControlUnit(this.instructionMemory, this.registerFile, this.dataMemory);
        this.alu = new DefaultArithmeticLogicUnit(this.registerFile);
        this.blu = new DefaultBranchLogicUnit(this.registerFile);
        this.sdtu = new DefaultSingleDataTransferUnit(this.registerFile, this.dataMemory);
    }

    public ControlUnit getControlUnit() {
        return controlUnit;
    }

    public SRamDataMemory getDataMemory() {
        return dataMemory;
    }

    public InstructionMemory getInstructionMemory() {
        return instructionMemory;
    }

    public RegisterFile getRegisterFile() {
        return registerFile;
    }

    public ArithmeticLogicUnit getAlu() {
        return alu;
    }

    public BranchLogicUnit getBlu() {
        return blu;
    }

    public SingleDataTransferUnit getSdtu() {
        return sdtu;
    }

}
