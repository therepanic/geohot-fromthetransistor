package com.therepanic.statement;

import com.therepanic.expression.Expression;

public record IfStatement(Expression cond, Statement thenBranch, Statement elseBranch) {
}
