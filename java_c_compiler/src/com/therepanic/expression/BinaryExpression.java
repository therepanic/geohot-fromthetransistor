package com.therepanic.expression;

import com.therepanic.Operator;

public record BinaryExpression(Expression left, Expression right, Operator op) implements Expression {
}
