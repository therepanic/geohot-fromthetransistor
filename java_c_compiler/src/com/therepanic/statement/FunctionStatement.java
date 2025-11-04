package com.therepanic.statement;

import com.therepanic.expression.Variable;

import java.util.List;

public record FunctionStatement(Variable returnType, String name, List<VarDeclaration> parameters, List<Statement> body) implements Statement {
}
