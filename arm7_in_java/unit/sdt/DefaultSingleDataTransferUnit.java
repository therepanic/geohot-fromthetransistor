package com.therepanic.unit.sdt;

import com.therepanic.dataMemory.SRamDataMemory;
import com.therepanic.registerFile.RegisterFile;
import com.therepanic.unit.Unit;
import com.therepanic.unit.UnitCondition;

public class DefaultSingleDataTransferUnit extends Unit implements SingleDataTransferUnit {

    private final SRamDataMemory dataMemory;

    public DefaultSingleDataTransferUnit(RegisterFile registerFile, SRamDataMemory dataMemory) {
        super(registerFile);
        this.dataMemory = dataMemory;
    }

    @Override
    public void execute(UnitCondition condition, boolean immediate, boolean pre, boolean up, boolean word,  boolean write, boolean load, int rn, int rd, int offset) {
        if (!checkCondition(condition)) return;
        int op1 = this.registerFile.read(rn);
        int signedOffset;
        if (immediate) {
            signedOffset = up ? offset : -offset;
        } else {
            // todo: shift for register
            int rm = this.registerFile.read(offset);
            signedOffset = up ? rm : -rm;
        }
        int addr = pre ? op1 + signedOffset : op1;
        if (load) {
            int val = word ? this.dataMemory.readWord(addr) : this.dataMemory.readByte(addr);
            this.registerFile.write(rd, val, false);
        } else {
            int val = this.registerFile.read(rd);
            if (word) {
                this.dataMemory.writeWord(addr, val);
            } else {
                this.dataMemory.writeByte(addr, (byte) val);
            }
        }
        if (write) {
            if (pre) {
                this.registerFile.write(rn, addr, false);
            } else {
                this.registerFile.write(rn, op1 + signedOffset, false);
            }
        }
    }

}
