package com.therepanic.statement;

import com.therepanic.expression.Expression;

public record IfStmt(Expression cond, Statement thenBranch, Statement elseBranch) {
}
