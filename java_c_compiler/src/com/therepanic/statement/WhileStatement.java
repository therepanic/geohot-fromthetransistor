package com.therepanic.statement;

import com.therepanic.expression.Expression;

import java.util.List;

public record WhileStatement(Expression cond, List<Statement> body) implements Statement {
}
