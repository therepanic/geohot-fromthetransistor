package com.therepanic.registerFile;

public abstract class AbstractARM7RegisterFile implements ARM7RegisterFile {

    protected static final int FLAG_N = 1 << 31;

    protected static final int FLAG_Z = 1 << 30;

    protected static final int FLAG_C = 1 << 29;

    protected static final int FLAG_V = 1 << 28;

    protected static final int PC_READ_OFFSET = 8;

}
