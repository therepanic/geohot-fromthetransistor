package com.therepanic.statement;

import java.util.List;

public record StructStatement(String name, List<VarDeclaration> fields, boolean typedef) implements Statement {
}
