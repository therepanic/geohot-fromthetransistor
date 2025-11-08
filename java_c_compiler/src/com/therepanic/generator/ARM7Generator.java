package com.therepanic.generator;

import com.therepanic.expression.*;
import com.therepanic.statement.FunctionStatement;
import com.therepanic.statement.Statement;
import com.therepanic.statement.StructStatement;
import com.therepanic.statement.VarDeclaration;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ARM7Generator implements Generator {

    private final Map<String, FunctionStatement> functions = new HashMap<>();


    @Override
    public List<String> generate(Statement statement) {
        if (statement instanceof FunctionStatement functionStatement) {
            this.functions.put(functionStatement.name(), functionStatement);
            return generateFunction(functionStatement);
        } else if (statement instanceof StructStatement structStatement) {
            // todo
            return List.of();
        } else {
            throw new IllegalArgumentException("Unknown statement " + statement);
        }
    }

    private List<String> generateFunction(FunctionStatement functionStatement) {
        Map<String, Integer> localsMap = new HashMap<>();
        Map<String, Integer> paramStackMap = new HashMap<>();
        int offset = 4;
        List<String> instructions = new ArrayList<>();
        instructions.add(functionStatement.name() + ":");
        instructions.add("  push {fp, lr}");
        instructions.add("  add fp, sp, #0");
        int argCount = functionStatement.parameters().size();
        for (int i = 0; i < Math.min(4, argCount); i++) {
            VarDeclaration p = functionStatement.parameters().get(i);
            int size = getTypeSize(p.type().baseType());
            localsMap.put(p.name(), offset);
            offset += size;
        }
        List<VarDeclaration> localsList = new ArrayList<>();
        for (Statement st : functionStatement.body()) {
            if (st instanceof VarDeclaration varDeclaration) {
                localsList.add(varDeclaration);
            }
        }
        for (VarDeclaration local : localsList) {
            String baseType = local.type().baseType();
            int size = getTypeSize(baseType);
            localsMap.put(local.name(), offset);
            offset += size;
        }
        if (offset > 4) {
            instructions.add("  sub sp, sp, #" + (offset - 4));
        }
        for (int i = 0; i < Math.min(4, argCount); i++) {
            VarDeclaration p = functionStatement.parameters().get(i);
            int slot = localsMap.get(p.name());
            instructions.add("  str r" + i + ", [fp, #-" + slot + "]");
        }
        for (int i = 4; i < argCount; i++) {
            VarDeclaration p = functionStatement.parameters().get(i);
            int posOff = 8 + 4 * (i - 4);
            paramStackMap.put(p.name(), posOff);
        }
        for (Statement statement : functionStatement.body()) {
            instructions.addAll(generateBodyStatement(statement, localsMap, paramStackMap));
        }
        if (offset > 4) {
            instructions.add("  add sp, sp, #" + (offset - 4));
        }
        instructions.add("  pop {fp, pc}");
        return instructions;
    }

    private List<String> generateBodyStatement(Statement statement,  Map<String, Integer> localsMap,  Map<String, Integer> paramStackMap) {
        List<String> instructions = new ArrayList<>();
        if (statement instanceof VarDeclaration varDeclaration) {
            if (varDeclaration.value() != null) {
                instructions.addAll(generateExpression(varDeclaration.value(), localsMap, paramStackMap));
                if (localsMap.containsKey(varDeclaration.name())) {
                    instructions.add("  str r0, [fp, #-" + localsMap.get(varDeclaration.name()) + "]");
                } else {
                    instructions.add("  str r0, [fp, #" + paramStackMap.get(varDeclaration.name()) + "]");
                }
            }
        }
        return instructions;
    }

    private List<String> generateExpression(Expression expression, Map<String, Integer> localsMap,  Map<String, Integer> paramStackMap) {
        List<String> instructions = new ArrayList<>();
        if (expression instanceof Literal literal) {
            if (literal instanceof IntLiteral(Integer value)) {
                return List.of("    mov r0, #" + value);
            } else {
                throw new IllegalStateException("Unexpected literal: " + literal);
            }
        } else if (expression instanceof Variable variable) {
            Integer slot = localsMap.get(variable.name());
            if (slot != null) {
                instructions.add("  ldr r0, [fp, #-" + slot + "]");
                return instructions;
            } else {
                Integer posOff = paramStackMap.get(variable.name());
                if (posOff == null) {
                    throw new IllegalStateException("Unknown variable or param: " + variable.name());
                }
                instructions.add("  ldr r0, [fp, #" + posOff + "]");
                return instructions;
            }
        } else if (expression instanceof BinaryExpression binaryExpression) {
            instructions.addAll(generateExpression(binaryExpression.left(), localsMap, paramStackMap));
            instructions.add("  push {r0}");
            instructions.addAll(generateExpression(binaryExpression.right(), localsMap, paramStackMap));
            instructions.add("  pop {r1}");
            String op = switch (binaryExpression.op()) {
                case PLUS -> "add";
                case MINUS -> "sub";
                case MUL -> "mul";
                case DIV -> "sdiv";
                default -> throw new IllegalStateException("Unexpected operation: " + binaryExpression.op());
            };
            instructions.add("  " + op + " r0, r1, r0");
            return instructions;
        } else {
            throw new IllegalStateException("Unexpected expression: " + expression);
        }
    }

    private int getTypeSize(String type) {
        return switch (type) {
            case "int", "float" -> 4;
            case "long" -> 8;
            default -> throw new IllegalStateException("Unexpected value: " + type);
        };
    }

}
