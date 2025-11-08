package com.therepanic.expression;

import java.util.List;

public record FunctionCallExpression(String name, List<Expression> args) implements Expression {

}
