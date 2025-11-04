package com.therepanic.statement;

import com.therepanic.expression.Expression;

import java.util.List;

public record IfStatement(Expression cond, List<Statement> thenBranch, List<Statement> elseBranch) implements Statement {
}
