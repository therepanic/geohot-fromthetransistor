package com.therepanic.lexer;

import com.therepanic.Token;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;

public interface Lexer {

    List<Token> tokenize(InputStream source);

    default List<Token> tokenize(String source) {
        return tokenize(new ByteArrayInputStream(source.getBytes(StandardCharsets.UTF_8)));
    }

}
