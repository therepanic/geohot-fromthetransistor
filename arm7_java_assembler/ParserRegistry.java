package com.therepanic;

import com.therepanic.parser.Parser;
import com.therepanic.parser.arm7.*;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

public class ParserRegistry {

    private static final Map<String, Supplier<? extends Parser>> FACTORIES = new HashMap<>();

    static {
        registerFactory(ARM7BranchParser::new);
        registerFactory(ARM7DataProcessingParser::new);
        registerFactory(ARM7MulMlaParser::new);
        registerFactory(ARM7MullMlalParser::new);
        registerFactory(ARM7SingleDataSwapParser::new);
        registerFactory(ARM7SoftwareInterruptParser::new);
        registerFactory(ARM7CoprocessorDataOperationsParser::new);
        registerFactory(ARM7CoprocessorRegisterTransfersParser::new);
        registerFactory(ARM7SingleDataTransferParser::new);
    }

    public static void registerFactory(Supplier<? extends Parser> factory) {
        Parser p = factory.get();
        for (String opcode : p.supportedOpcodes()) {
            FACTORIES.putIfAbsent(opcode, factory);
        }
    }

    public static Parser createForOpcode(String opcode) {
        Supplier<? extends Parser> f = FACTORIES.get(opcode.toUpperCase());
        return f == null ? null : f.get();
    }

}
