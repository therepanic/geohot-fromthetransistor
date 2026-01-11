package com.therepanic.parser.arm7;

import com.therepanic.parser.Parser;

import java.util.Map;

public abstract class ARM7Parser implements Parser {

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

    protected Integer getConditionCode(String mnemonic) {
        return COND_CODES.get(mnemonic);
    }

    protected Integer getConditionCodeOrDefault(String mnemonic, Integer defaultValue) {
        return COND_CODES.getOrDefault(mnemonic, defaultValue);
    }

}
