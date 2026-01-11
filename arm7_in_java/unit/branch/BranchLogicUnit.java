package com.therepanic.unit.branch;

import com.therepanic.unit.UnitCondition;

public interface BranchLogicUnit {

    void execute(UnitCondition condition, boolean link, int offset);

}
