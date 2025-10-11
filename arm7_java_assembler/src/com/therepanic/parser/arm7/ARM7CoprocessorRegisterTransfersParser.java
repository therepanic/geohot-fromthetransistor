package com.therepanic.parser.arm7;

import com.therepanic.Instruction;

import java.util.List;
import java.util.Map;
import java.util.Set;

public class ARM7CoprocessorRegisterTransfersParser extends ARM7Parser {

    private static final Map<String, Integer> COND_CODES = Map.ofEntries(
            Map.entry("EQ", 0b0000),
            Map.entry("NE", 0b0001),
            Map.entry("CS", 0b0010),
            Map.entry("CC", 0b0011),
            Map.entry("MI", 0b0100),
            Map.entry("PL", 0b0101),
            Map.entry("VS", 0b0110),
            Map.entry("VC", 0b0111),
            Map.entry("HI", 0b1000),
            Map.entry("LS", 0b1001),
            Map.entry("GE", 0b1010),
            Map.entry("LT", 0b1011),
            Map.entry("GT", 0b1100),
            Map.entry("LE", 0b1101)
    );

    @Override
    public Instruction parse(List<String> tokens) {
        String op = tokens.getFirst();
        int condCode;
        if (op.length() > 3) {
            condCode = COND_CODES.get(op.substring(3));
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
        instruction |= opc1 << 21;
        instruction |= op.startsWith("MRC") ? 1 << 20 : 0;
        instruction |= cRn << 16;
        instruction |= cRd << 12;
        instruction |= coProc << 8;
        instruction |= opc2 << 5;
        instruction |= 0b1 << 4;
        instruction |= cRm;
        return new Instruction(instruction);
    }

    @Override
    public Set<String> supportedOpcodes() {
        return Set.of("MRC", "MCR");
    }

}
