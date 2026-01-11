package com.therepanic.dataMemory;

public interface SRamDataMemory {

    void writeWord(int address, int data);

    int readWord(int address);

    void writeByte(int address, byte data);

    byte readByte(int address);

}
