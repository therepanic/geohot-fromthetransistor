package com.therepanic.unit.branch;

import com.therepanic.registerFile.RegisterFile;
import com.therepanic.unit.Unit;
import com.therepanic.unit.UnitCondition;

public class DefaultBranchLogicUnit extends Unit implements BranchLogicUnit {

    public DefaultBranchLogicUnit(RegisterFile registerFile) {
        super(registerFile);
    }

    @Override
    public void execute(UnitCondition condition, boolean link, int offset) {
        if (this.checkCondition(condition)) {
            if (link) {
                this.registerFile.write(14, this.registerFile.readForFetch() + 4, false);
            }
            int off24 = offset & 0x00FFFFFF;
            int signedOff = (off24 << 8) >> 8;
            int branchOffset = signedOff << 2;
            this.registerFile.write(15, this.registerFile.readOperand() + branchOffset, false);
        } else {
            this.registerFile.write(15, this.registerFile.readForFetch() + 4, false);
        }
    }

}
