package com.therepanic.lexer;

import com.therepanic.Token;

import java.util.List;

public interface Lexer {

    List<Token> tokenize(String source);

}
