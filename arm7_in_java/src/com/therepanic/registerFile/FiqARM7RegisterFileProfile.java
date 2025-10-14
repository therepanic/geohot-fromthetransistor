package com.therepanic.registerFile;

public class FiqARM7RegisterFileProfile extends ARM7RegisterFileProfile {

    private final int[] registers = new int[5];

    public FiqARM7RegisterFileProfile(int sp, int lr) {
        super(sp, lr);
    }

    public int[] getRegisters() {
        return registers;
    }

}
