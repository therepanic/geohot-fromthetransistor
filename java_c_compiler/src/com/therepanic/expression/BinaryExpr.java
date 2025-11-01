package com.therepanic.expression;

import com.therepanic.Operator;

public record BinaryExpr(Expression left, Expression right, Operator op) implements Expression {
}
