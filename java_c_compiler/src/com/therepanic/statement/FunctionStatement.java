package com.therepanic.statement;

import com.therepanic.type.Type;

import java.util.List;

public record FunctionStatement(Type returnType, String name, List<VarDeclaration> parameters, List<Statement> body) implements Statement {
}
