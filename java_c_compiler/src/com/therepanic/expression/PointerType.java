package com.therepanic.expression;

public record PointerType(String baseType, int depth) implements Expression {
}
