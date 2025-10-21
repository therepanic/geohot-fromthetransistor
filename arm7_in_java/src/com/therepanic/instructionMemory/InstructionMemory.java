package com.therepanic.instructionMemory;

public interface InstructionMemory {

    void addInstruction(int bit);

    int readInstruction(int pc);

    int size();

}
