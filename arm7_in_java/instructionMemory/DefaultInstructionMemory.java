package com.therepanic.instructionMemory;

public class DefaultInstructionMemory extends AbstractInstructionMemory {

    private final int[] memory = new int[MEMORY_SIZE];
    private int pointer;

    @Override
    public void addInstruction(int bit) {
        if (this.pointer == this.memory.length - 1) {
            throw new RuntimeException("Not enough memory");
        }
        this.memory[this.pointer] = bit;
        this.pointer++;
    }

    @Override
    public int readInstruction(int pc) {
        return this.memory[pc / 4];
    }

    @Override
    public int size() {
        return this.memory.length - this.pointer + 1;
    }

}
