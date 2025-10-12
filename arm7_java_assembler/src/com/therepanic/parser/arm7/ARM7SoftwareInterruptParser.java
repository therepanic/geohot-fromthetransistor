package com.therepanic.parser.arm7;

import com.therepanic.Instruction;

import java.util.List;
import java.util.Set;

public class ARM7SoftwareInterruptParser extends ARM7Parser {

    @Override
    public Instruction parse(List<String> tokens) {
        System.out.println(tokens);
        String op = tokens.getFirst().toUpperCase();
        int condCode = op.length() > 3 ? getConditionCodeOrDefault(op.substring(3), 0b1110) : 0b1110;
        int instruction = 0;
        instruction |= condCode << 28;
        instruction |= 0b1111 << 24;
        int comment;
        if (tokens.get(1).startsWith("0X")) {
            comment = Integer.parseInt(tokens.get(1).substring(2), 16);
        } else {
            comment = Integer.parseInt(tokens.get(1));
        }
        instruction |= comment & 0xFFFFFF;
        return new Instruction(instruction);
    }

    @Override
    public Set<String> supportedOpcodes() {
        return Set.of("SWI");
    }

}
