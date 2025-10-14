package com.therepanic.registerFile;

import java.util.Map;

public interface ARM7RegisterFile {

    int read(int reg);

    void write(int reg, int value, boolean restoreFromSPSR);

    int readForFetch();

    int readOperand();

    void setPC(int value);

    boolean getFlagN();

    boolean getFlagZ();

    boolean getFlagC();

    boolean getFlagV();

    void setFlagN(boolean n);

    void setFlagZ(boolean z);

    void setFlagC(boolean c);

    void setFlagV(boolean v);

    int getCPSR();

    Map<ARM7Mode, Integer> getSPSR();

    void setCPSR(int value);

    void setFlagsFromResult(int result, boolean carry, boolean overflow);

    ARM7Mode getCurrentMode();
}
