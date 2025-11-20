package com.therepanic.type;

public record ArrayType(PrimitiveType baseType, int length) implements Type {
}
