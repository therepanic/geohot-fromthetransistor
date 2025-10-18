package com.therepanic.unit;

import com.therepanic.registerFile.ARM7RegisterFile;

public abstract class Unit {

    protected final ARM7RegisterFile registerFile;

    public Unit(ARM7RegisterFile registerFile) {
        this.registerFile = registerFile;
    }

    protected boolean checkCondition(UnitCondition cond) {
        boolean N = registerFile.getFlagN();
        boolean Z = registerFile.getFlagZ();
        boolean C = registerFile.getFlagC();
        boolean V = registerFile.getFlagV();
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
