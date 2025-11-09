package com.therepanic;

import com.therepanic.generator.ARM7Generator;
import com.therepanic.generator.Generator;
import com.therepanic.lexer.Lexer;
import com.therepanic.lexer.SimpleLexer;
import com.therepanic.parser.Parser;
import com.therepanic.parser.SimpleParser;
import com.therepanic.preprocessor.Preprocessor;
import com.therepanic.preprocessor.SimplePreprocessor;
import com.therepanic.statement.Statement;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class Main {
    public static void main(String[] args) throws IOException {
        String str = Files.readString(Path.of("D:\\iverilog\\main.c"));
        Preprocessor preprocessor = new SimplePreprocessor();
        str = preprocessor.process(str);
        Lexer lexer = new SimpleLexer();
        List<Token> tokens = lexer.tokenize(str);
        System.out.println(tokens);
        Parser parser = new SimpleParser();
        System.out.println(parser.parse(tokens));
        Generator generator = new ARM7Generator();
        for (Statement statement : parser.parse(tokens)) {
            System.out.println(generator.generate(statement));
        }
    }
}
