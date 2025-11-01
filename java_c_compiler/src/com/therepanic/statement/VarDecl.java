package com.therepanic.statement;

import com.therepanic.expression.Expression;

public record VarDecl(String name, Expression value) implements Statement {
}
