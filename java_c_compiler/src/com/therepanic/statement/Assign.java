package com.therepanic.statement;

import com.therepanic.expression.Expression;

public record Assign(Expression lhs, Expression rhs) implements Statement {
}
