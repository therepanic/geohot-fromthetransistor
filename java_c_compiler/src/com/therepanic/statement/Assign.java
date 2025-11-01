package com.therepanic.statement;

import com.therepanic.expression.Expression;

public record Assign(String name, Expression value) implements Statement {
}
