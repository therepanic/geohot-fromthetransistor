package com.therepanic.statement;

import com.therepanic.expression.Expression;


import java.util.List;

public record FunctionCallStatement(String name, List<Expression> args) implements Statement {
}
