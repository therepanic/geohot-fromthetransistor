package com.therepanic.generator;

import com.therepanic.statement.Statement;

import java.util.List;

public interface Generator {

    List<String> generate(Statement statement);

}
