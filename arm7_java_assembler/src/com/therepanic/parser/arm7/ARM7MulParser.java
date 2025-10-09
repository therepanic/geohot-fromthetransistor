package com.therepanic.parser.arm7;

import com.therepanic.Instruction;

import java.util.List;
import java.util.Set;

public class ARM7MulParser extends ARM7Parser {

    @Override
    public Instruction parse(List<String> tokens) {
        return null;
    }

    @Override
    public Set<String> supportedOpcodes() {
        return Set.of();
    }

}
