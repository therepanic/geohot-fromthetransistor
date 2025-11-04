package com.therepanic.compiler;

import com.therepanic.statement.Statement;

public interface Compiler {

    String generate(Statement statement);

}
