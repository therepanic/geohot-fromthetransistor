package com.therepanic.statement;

import com.therepanic.expression.Expression;

public record VarDeclaration(String name, Expression value) implements Statement {
}
