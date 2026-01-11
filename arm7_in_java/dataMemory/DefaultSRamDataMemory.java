package com.therepanic.dataMemory;

public class DefaultSRamDataMemory extends AbstractSRamDataMemory {

    private final int[] memory = new int[MEMORY_SIZE];

    @Override
    public void writeWord(int address, int data) {
        this.memory[address >> 2] = data;
    }

    @Override
    public int readWord(int address) {
        return this.memory[address >> 2];
    }

    @Override
    public void writeByte(int address, byte data) {
        int wordIndex = address >> 2;
        int byteOffset = (address & 0b11) * 8;
        int mask = 0xFF << byteOffset;
        this.memory[wordIndex] = (this.memory[wordIndex] & ~mask) | ((data & 0xFF) << byteOffset);
    }

    @Override
    public byte readByte(int address) {
        int wordIndex = address >> 2;
        int byteOffset = (address & 0b11) * 8;
        return (byte) ((this.memory[wordIndex] >> byteOffset) & 0xFF);
    }

}
