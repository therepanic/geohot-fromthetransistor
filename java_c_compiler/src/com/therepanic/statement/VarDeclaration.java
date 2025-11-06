package com.therepanic.statement;

import com.therepanic.expression.Expression;
import com.therepanic.expression.PointerType;

public record VarDeclaration(String name, Expression value, PointerType type) implements Statement {
}
