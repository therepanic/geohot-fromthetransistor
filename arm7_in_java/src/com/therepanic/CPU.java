package com.therepanic;

import java.util.List;

public class CPU {

    private final DataPath dataPath = new DataPath();

    public void execute(List<Integer> instructions) {
        for (int instruction : instructions) {
            this.dataPath.getInstructionMemory().addInstruction(instruction);
            int instr = this.dataPath.getControlUnit().fetch();
            this.dataPath.getControlUnit().decodeAndExecute(instr);
        }
    }

}
