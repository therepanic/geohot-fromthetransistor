package com.therepanic.unit;

import com.therepanic.registerFile.RegisterFile;

public abstract class Unit {

    protected final RegisterFile registerFile;

    public Unit(RegisterFile registerFile) {
        this.registerFile = registerFile;
    }

    protected boolean checkCondition(UnitCondition cond) {
        boolean N = this.registerFile.getFlagN();
        boolean Z = this.registerFile.getFlagZ();
        boolean C = this.registerFile.getFlagC();
        boolean V = this.registerFile.getFlagV();
        return switch (cond) {
            case EQ -> Z;
            case NE -> !Z;
            case CS -> C;
            case CC -> !C;
            case MI -> N;
            case PL -> !N;
            case VS -> V;
            case VC -> !V;
            case HI -> C && !Z;
            case LS -> !C || Z;
            case GE -> N == V;
            case LT -> N != V;
            case GT -> !Z && (N == V);
            case LE -> Z || (N != V);
            case ALL -> true;
        };
    }

}
