package com.therepanic.expression;

public record ArrayAccessExpression(String name, Expression index) implements Expression {
}
