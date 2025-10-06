package com.therepanic.parser;

import com.therepanic.Instruction;

import java.util.List;

public interface Parser {

    Instruction parse(List<String> tokens);

}
