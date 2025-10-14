package com.therepanic.registerFile;

import java.util.EnumMap;
import java.util.Map;

public class DefaultARM7RegisterFile extends AbstractARM7RegisterFile {

    private final int[] registers = new int[13];

    private int pc;

    private int cpsr;

    private final Map<ARM7Mode, ARM7RegisterFileProfile> registerFileProfiles = new EnumMap<>(ARM7Mode.class);

    private final Map<ARM7Mode, Integer> spsr = new EnumMap<>(ARM7Mode.class);

    public DefaultARM7RegisterFile() {
        ARM7RegisterFileProfile all = new ARM7RegisterFileProfile(0, 0);
        this.registerFileProfiles.put(ARM7Mode.USER, all);
        this.registerFileProfiles.put(ARM7Mode.SYS, all);
        this.registerFileProfiles.put(ARM7Mode.FIQ, new FiqARM7RegisterFileProfile(0, 0));
        this.registerFileProfiles.put(ARM7Mode.IRQ, new ARM7RegisterFileProfile(0, 0));
        this.registerFileProfiles.put(ARM7Mode.SVC, new ARM7RegisterFileProfile(0, 0));
        this.registerFileProfiles.put(ARM7Mode.ABT, new ARM7RegisterFileProfile(0, 0));
        this.registerFileProfiles.put(ARM7Mode.UND, new ARM7RegisterFileProfile(0, 0));

        this.spsr.put(ARM7Mode.FIQ, 0);
        this.spsr.put(ARM7Mode.IRQ, 0);
        this.spsr.put(ARM7Mode.SVC, 0);
        this.spsr.put(ARM7Mode.ABT, 0);
        this.spsr.put(ARM7Mode.UND, 0);
    }

    @Override
    public int read(int reg) {
        if (getMode().equals(ARM7Mode.FIQ) && reg >= 8 && reg <= 12) {
            FiqARM7RegisterFileProfile profile = (FiqARM7RegisterFileProfile)
                    this.registerFileProfiles.get(getMode());
            return profile.getRegisters()[reg - 8];
        }
        return readSpecialRegister(reg);
    }

    @Override
    public void write(int reg, int value, boolean restoreFromSPSR) {
        if (getMode().equals(ARM7Mode.FIQ) && reg >= 8 && reg <= 12) {
            FiqARM7RegisterFileProfile profile = (FiqARM7RegisterFileProfile)
                    this.registerFileProfiles.get(getMode());
            profile.getRegisters()[reg - 8] = value;
            return;
        }
        if (reg == 15) {
            writePC(value, restoreFromSPSR);
            return;
        }
        writeSpecialRegister(reg, value);
    }

    @Override
    public int readForFetch() {
        return this.pc;
    }

    @Override
    public int readOperand() {
        return this.pc + PC_READ_OFFSET;
    }

    @Override
    public void setPC(int value) {
        this.pc = value;
    }

    @Override
    public boolean getFlagN() {
        return (this.cpsr & FLAG_N) != 0;
    }

    @Override
    public boolean getFlagZ() {
        return (this.cpsr & FLAG_Z) != 0;
    }

    @Override
    public boolean getFlagC() {
        return (this.cpsr & FLAG_C) != 0;
    }

    @Override
    public boolean getFlagV() {
        return (this.cpsr & FLAG_V) != 0;
    }

    @Override
    public void setFlagN(boolean n) {
        this.cpsr = n ? (this.cpsr | FLAG_N) : (this.cpsr & ~FLAG_N);
    }

    @Override
    public void setFlagZ(boolean z) {
        this.cpsr = z ? (this.cpsr | FLAG_Z) : (this.cpsr & ~FLAG_Z);
    }

    @Override
    public void setFlagC(boolean c) {
        this.cpsr = c ? (this.cpsr | FLAG_C) : (this.cpsr & ~FLAG_C);
    }

    @Override
    public void setFlagV(boolean v) {
        this.cpsr = v ? (this.cpsr | FLAG_V) : (this.cpsr & ~FLAG_V);
    }

    @Override
    public int getCPSR() {
        return this.cpsr;
    }

    @Override
    public Map<ARM7Mode, Integer> getSPSR() {
        return this.spsr;
    }

    @Override
    public void setCPSR(int value) {
        this.cpsr = value;
    }

    @Override
    public void setFlagsFromResult(int result, boolean carry, boolean overflow) {
        setFlagZ(result == 0);
        setFlagN((result & 0x80000000) != 0);
        setFlagC(carry);
        setFlagV(overflow);
    }

    @Override
    public ARM7Mode getCurrentMode() {
        return getMode();
    }

    private int readSpecialRegister(int reg) {
        if (reg == 13) {
            return this.registerFileProfiles.get(getMode()).getSp();
        } else if (reg == 14) {
            return this.registerFileProfiles.get(getMode()).getLr();
        } else if (reg == 15) {
            return this.pc;
        }
        return this.registers[reg];
    }

    private void writeSpecialRegister(int reg, int value) {
        if (reg == 13) {
            this.registerFileProfiles.get(getMode()).setSp(value);
            return;
        } else if (reg == 14) {
            this.registerFileProfiles.get(getMode()).setLr(value);
            return;
        } else if (reg == 15) {
            this.pc = value;
            return;
        }
        this.registers[reg] = value;
    }

    private void writePC(int value, boolean restoreFromSPSR) {
        int effective = value & ~3;
        //note: we not support thumb now
        if (restoreFromSPSR) {
            Integer savedCPSR = spsr.get(getMode());
            if (savedCPSR != null) {
                this.cpsr = savedCPSR;
            }
        }
        this.pc = effective;
    }

    private ARM7Mode getMode() {
        int modeBits = this.cpsr & 0b11111;
        return switch (modeBits) {
            case 0b10000 -> ARM7Mode.USER;
            case 0b10001 -> ARM7Mode.FIQ;
            case 0b10010 -> ARM7Mode.IRQ;
            case 0b10011 -> ARM7Mode.SVC;
            case 0b10111 -> ARM7Mode.ABT;
            case 0b11011 -> ARM7Mode.UND;
            case 0b11111 -> ARM7Mode.SYS;
            default -> throw new IllegalStateException("Unknown mode: " + modeBits);
        };
    }

}
