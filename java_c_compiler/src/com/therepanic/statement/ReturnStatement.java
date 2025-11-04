package com.therepanic.statement;

import com.therepanic.expression.Expression;

public record ReturnStatement(Expression value) implements Statement {
}
