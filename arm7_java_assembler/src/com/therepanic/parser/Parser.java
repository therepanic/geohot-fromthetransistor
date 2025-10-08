package com.therepanic.parser;

import com.therepanic.Instruction;

import java.util.List;
import java.util.Set;

public interface Parser {

    Instruction parse(List<String> tokens);

    Set<String> supportedOpcodes();

}
