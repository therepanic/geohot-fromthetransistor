package com.therepanic.parser;

import com.therepanic.Instruction;

public interface Parser {

    Instruction parse(String[] entry);

}
