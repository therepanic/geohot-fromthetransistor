package com.therepanic.statement;

import com.therepanic.expression.Expression;

public record WhileStmt(Expression cond, Statement body) implements Statement {
}
