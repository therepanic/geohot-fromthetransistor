package com.therepanic.unit.alu;

import com.therepanic.unit.UnitCondition;

public interface ArithmeticLogicUnit {

    void execute(UnitCondition condition,
                 DataProcessingOperation operation,
                 int rn,
                 int rd,
                 int operand2,
                 boolean immediate,
                 boolean s);

}
