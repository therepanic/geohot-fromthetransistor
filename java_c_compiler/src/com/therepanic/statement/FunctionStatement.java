package com.therepanic.statement;

import com.therepanic.expression.PointerType;

import java.util.List;

public record FunctionStatement(PointerType returnType, String name, List<VarDeclaration> parameters, List<Statement> body) implements Statement {
}
