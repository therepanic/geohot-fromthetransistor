package com.therepanic.registerFile;

public class FiqRegisterFileProfile extends RegisterFileProfile {

    private final int[] registers = new int[5];

    public FiqRegisterFileProfile(int sp, int lr) {
        super(sp, lr);
    }

    public int[] getRegisters() {
        return registers;
    }

}
