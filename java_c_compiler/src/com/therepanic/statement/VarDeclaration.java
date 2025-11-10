package com.therepanic.statement;

import com.therepanic.expression.Expression;
import com.therepanic.type.Type;

public record VarDeclaration(String name, Expression value, Type type) implements Statement {
}
