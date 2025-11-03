package com.therepanic.statement;

import com.therepanic.expression.Expression;

public record WhileStatement(Expression cond, Statement body) implements Statement {
}
