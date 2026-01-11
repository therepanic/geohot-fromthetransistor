package com.therepanic.unit.sdt;

import com.therepanic.unit.UnitCondition;

public interface SingleDataTransferUnit {

    void execute(UnitCondition condition,
                 boolean immediate,
                 boolean pre,
                 boolean up,
                 boolean word,
                 boolean write,
                 boolean load,
                 int rn,
                 int rd,
                 int offset);

}
