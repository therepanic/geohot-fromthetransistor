package com.therepanic;

import java.util.Objects;

public class Instruction {

    private final int bit;

    public Instruction(int bit) {
        this.bit = bit;
    }

    public int getBit() {
        return this.bit;
    }

    @Override
    public boolean equals(Object o) {
        if (o == null || getClass() != o.getClass()) return false;
        Instruction that = (Instruction) o;
        return this.bit == that.bit;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(this.bit);
    }

}