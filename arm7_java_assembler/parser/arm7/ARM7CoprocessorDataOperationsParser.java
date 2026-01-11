package com.therepanic.parser.arm7;

import com.therepanic.Instruction;

import java.util.List;
import java.util.Set;

public class ARM7CoprocessorDataOperationsParser extends ARM7Parser {

    @Override
    public Instruction parse(List<String> tokens) {
        String op = tokens.getFirst();
        int condCode;
        if (op.length() > 3) {
            condCode = getConditionCode(op.substring(3));
        } else {
            //AL cond code
            condCode = 0b1110;
        }
        int coProc = Integer.parseInt(tokens.get(1).substring(1));
        int opc1 = Integer.parseInt(tokens.get(2));
        int cRd = Integer.parseInt(tokens.get(3).substring(1));
        int cRn = Integer.parseInt(tokens.get(4).substring(1));
        int cRm = Integer.parseInt(tokens.get(5).substring(1));
        int opc2 = Integer.parseInt(tokens.get(6));
        int instruction = 0;
        instruction |= condCode << 28;
        instruction |= 0b1110 << 24;
        instruction |= opc1 << 20;
        instruction |= cRn << 16;
        instruction |= cRd << 12;
        instruction |= coProc << 8;
        instruction |= opc2 << 5;
        instruction |= 0b0 << 4;
        instruction |= cRm;
        return new Instruction(instruction);
    }

    @Override
    public Set<String> supportedOpcodes() {
        return Set.of("CDP");
    }

}
