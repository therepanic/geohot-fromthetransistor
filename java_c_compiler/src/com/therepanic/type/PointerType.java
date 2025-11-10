package com.therepanic.type;

public record PointerType(PrimitiveType baseType, int depth) implements Type {
}
