package com.therepanic.parser;

import com.therepanic.Token;
import com.therepanic.statement.Statement;

import java.util.List;

public interface Parser {

    List<Statement> parse(List<Token> tokens);

}
