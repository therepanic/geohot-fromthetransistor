package com.therepanic.generator;

import com.therepanic.statement.Statement;

import java.util.List;

public class SimpleGenerator implements Generator {

    @Override
    public List<String> generate(Statement statement) {
        return List.of();
    }

}
