package com.therepanic.statement;

import com.therepanic.expression.Expression;

import java.util.List;

public record FunctionStatement(String name, Expression returnType, List<VarDeclaration> parameters, List<Statement> body) {
}
