package com.therepanic.registerFile;

import java.util.EnumMap;
import java.util.Map;

public class DefaultRegisterFile extends AbstractRegisterFile {

    private final int[] registers = new int[13];

    private int pc;

    private int cpsr;

    private final Map<Mode, RegisterFileProfile> registerFileProfiles = new EnumMap<>(Mode.class);

    private final Map<Mode, Integer> spsr = new EnumMap<>(Mode.class);

    public DefaultRegisterFile() {
        RegisterFileProfile all = new RegisterFileProfile(0, 0);
        this.registerFileProfiles.put(Mode.USER, all);
        this.registerFileProfiles.put(Mode.SYS, all);
        this.registerFileProfiles.put(Mode.FIQ, new FiqRegisterFileProfile(0, 0));
        this.registerFileProfiles.put(Mode.IRQ, new RegisterFileProfile(0, 0));
        this.registerFileProfiles.put(Mode.SVC, new RegisterFileProfile(0, 0));
        this.registerFileProfiles.put(Mode.ABT, new RegisterFileProfile(0, 0));
        this.registerFileProfiles.put(Mode.UND, new RegisterFileProfile(0, 0));

        this.spsr.put(Mode.FIQ, 0);
        this.spsr.put(Mode.IRQ, 0);
        this.spsr.put(Mode.SVC, 0);
        this.spsr.put(Mode.ABT, 0);
        this.spsr.put(Mode.UND, 0);
    }

    @Override
    public int read(int reg) {
        if (getMode().equals(Mode.FIQ) && reg >= 8 && reg <= 12) {
            FiqRegisterFileProfile profile = (FiqRegisterFileProfile)
                    this.registerFileProfiles.get(getMode());
            return profile.getRegisters()[reg - 8];
        }
        return readSpecialRegister(reg);
    }

    @Override
    public void write(int reg, int value, boolean restoreFromSPSR) {
        if (getMode().equals(Mode.FIQ) && reg >= 8 && reg <= 12) {
            FiqRegisterFileProfile profile = (FiqRegisterFileProfile)
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
    public Map<Mode, Integer> getSPSR() {
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
    public Mode getCurrentMode() {
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

    private Mode getMode() {
        int modeBits = this.cpsr & 0b11111;
        return switch (modeBits) {
            case 0b10000 -> Mode.USER;
            case 0b10001 -> Mode.FIQ;
            case 0b10010 -> Mode.IRQ;
            case 0b10011 -> Mode.SVC;
            case 0b10111 -> Mode.ABT;
            case 0b11011 -> Mode.UND;
            case 0b11111 -> Mode.SYS;
            default -> throw new IllegalStateException("Unknown mode: " + modeBits);
        };
    }

}
