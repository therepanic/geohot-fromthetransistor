package com.therepanic.generator;

import com.therepanic.TypeResolver;
import com.therepanic.expression.*;
import com.therepanic.statement.*;
import com.therepanic.type.PointerType;
import com.therepanic.type.PrimitiveType;
import com.therepanic.type.Type;

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
            int size;
            if (p.type() instanceof PointerType ptr) {
                size = getTypeSize(ptr.baseType());
            } else {
                size = getTypeSize((PrimitiveType) p.type());
            }
            //alignment
            if (size == 8 && offset % 8 != 0) {
                offset += 4;
            }
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
            PrimitiveType baseType;
            if (local.type() instanceof PointerType ptr) {
                baseType = ptr.baseType();
            } else {
                baseType = (PrimitiveType) local.type();
            }
            int size = getTypeSize(baseType);
            //alignment
            if (size == 8 && offset % 8 != 0) {
                offset += 4;
            }
            localsMap.put(local.name(), offset);
            offset += size;
        }
        if (offset > 4) {
            instructions.add("  sub sp, sp, #" + (offset - 4));
        }
        int regIndex = 0;
        for (int i = 0; i < Math.min(4, argCount); i++) {
            VarDeclaration p = functionStatement.parameters().get(i);
            int slot = localsMap.get(p.name());
            PrimitiveType type = null;
            if (p.type() instanceof PrimitiveType primitiveType) {
                type = primitiveType;
            } else if (p.type() instanceof PointerType pointerType) {
                type = pointerType.baseType();
            }
            switch (type) {
                case INT -> {
                    instructions.add("  str r" + regIndex + ", [fp, #-" + slot + "]");
                    regIndex += 1;
                }
                case LONG -> {
                    instructions.add("  str r" + regIndex + ", [fp, #-" + slot + "]");
                    instructions.add("  str r" + (regIndex + 1) + ", [fp, #-" + (slot + 4) + "]");
                    regIndex += 2;
                }
                case FLOAT -> {
                    // todo
                }
                case null, default -> throw new IllegalStateException("Unexpected type: " + type);
            }
        }
        int runningPos = 8;
        for (int i = 4; i < argCount; i++) {
            VarDeclaration p = functionStatement.parameters().get(i);
            int size;
            if (p.type() instanceof PointerType ptr) {
                size = getTypeSize(ptr.baseType());
            } else if (p.type() instanceof PrimitiveType primitive) {
                size = getTypeSize(primitive);
            } else {
                size = 4;
            }
            paramStackMap.put(p.name(), runningPos);
            runningPos += size;
        }

        // local types searching
        Map<String, Type> localsTypes = getLocalsFromFunctionStatement(functionStatement);

        for (Statement statement : functionStatement.body()) {
            instructions.addAll(generateBodyStatement(functionStatement.name(), statement, localsMap, paramStackMap, localsTypes));
        }
        instructions.add(functionStatement.name() + "_end:");
        if (offset > 4) {
            instructions.add("  add sp, sp, #" + (offset - 4));
        }
        instructions.add("  pop {fp, pc}");
        return instructions;
    }

    private Map<String, Type> getLocalsFromFunctionStatement(FunctionStatement functionStatement) {
        Map<String, Type> localsTypes = new HashMap<>();
        for (VarDeclaration p : functionStatement.parameters()) {
            Type type;
            if (p.type() instanceof PrimitiveType primitive) {
                type = primitive;
            } else {
                type = ((PointerType) p.type()).baseType();
            }
            localsTypes.put(p.name(), type);
        }
        for (Statement s : functionStatement.body()) {
            if (s instanceof VarDeclaration vd) {
                Type type;
                if (vd.type() instanceof PrimitiveType primitive) {
                    type = primitive;
                } else {
                    type = ((PointerType) vd.type()).baseType();
                }
                localsTypes.put(vd.name(), type);
            }
        }
        return localsTypes;
    }

    private List<String> generateBodyStatement(String functionName, Statement statement, Map<String, Integer> localsMap, Map<String, Integer> paramStackMap, Map<String, Type> localsTypes) {
        List<String> instructions = new ArrayList<>();
        if (statement instanceof VarDeclaration varDeclaration) {
            if (varDeclaration.value() != null) {
                PrimitiveType allType = TypeResolver.inferType(varDeclaration.value(), localsTypes, this.functions);
                instructions.addAll(generateExpression(varDeclaration.value(), localsMap, paramStackMap, allType));
                switch (allType) {
                    case INT -> {
                        if (localsMap.containsKey(varDeclaration.name())) {
                            instructions.add("  str r0, [fp, #-" + localsMap.get(varDeclaration.name()) + "]");
                        } else {
                            instructions.add("  str r0, [fp, #" + paramStackMap.get(varDeclaration.name()) + "]");
                        }
                    }
                    case LONG -> {
                        if (localsMap.containsKey(varDeclaration.name())) {
                            instructions.add("  str r0, [fp, #-" + localsMap.get(varDeclaration.name()) + "]");
                            instructions.add("  str r1, [fp, #-" + (localsMap.get(varDeclaration.name()) + 4) + "]");
                        } else {
                            instructions.add("  str r0, [fp, #" + paramStackMap.get(varDeclaration.name()) + "]");
                            instructions.add("  str r1, [fp, #" + (paramStackMap.get(varDeclaration.name()) + 4) + "]");
                        }
                    }
                    case FLOAT -> {
                        //todo
                    }
                }
            }
        } else if (statement instanceof Assign assign) {
            if (assign.lhs() instanceof Variable variable) {
                PrimitiveType allType = TypeResolver.inferType(assign.rhs(), localsTypes, this.functions);
                instructions.addAll(generateExpression(assign.rhs(), localsMap, paramStackMap, allType));
                switch (allType) {
                    case INT -> {
                        if (localsMap.containsKey(variable.name())) {
                            instructions.add("  str r0, [fp, #-" + localsMap.get(variable.name()) + "]");
                        } else {
                            instructions.add("  str r0, [fp, #" + paramStackMap.get(variable.name()) + "]");
                        }
                    }
                    case LONG -> {
                        if (localsMap.containsKey(variable.name())) {
                            instructions.add("  str r0, [fp, #-" + localsMap.get(variable.name()) + "]");
                            instructions.add("  str r1, [fp, #-" + (localsMap.get(variable.name()) + 4) + "]");
                        } else {
                            instructions.add("  str r0, [fp, #" + paramStackMap.get(variable.name()) + "]");
                            instructions.add("  str r1, [fp, #" + (paramStackMap.get(variable.name()) + 4) + "]");
                        }
                    }
                    case FLOAT -> {
                        //todo
                    }
                }
            }
        } else if (statement instanceof ReturnStatement returnStatement) {
            if (returnStatement.value() != null) {
                PrimitiveType allType = TypeResolver.inferType(returnStatement.value(), localsTypes, this.functions);
                instructions.addAll(generateExpression(returnStatement.value(), localsMap, paramStackMap, allType));
            } else {
                instructions.add("  mov r0, #0");
                instructions.add("  mov r1, #0");
            }
            instructions.add("  b " + functionName + "_end");
        }
        return instructions;
    }

    private List<String> generateExpression(Expression expression, Map<String, Integer> localsMap,  Map<String, Integer> paramStackMap, PrimitiveType allType) {
        List<String> instructions = new ArrayList<>();
        if (expression instanceof Literal literal) {
            switch (allType) {
                case INT -> {
                    int val = 0;
                    if (literal instanceof IntLiteral(Integer value)) {
                        val = value;
                    } else if (literal instanceof LongLiteral(Long value)) {
                        val = value.intValue();
                    } else if (literal instanceof FloatLiteral(Float value)) {
                        val = value.intValue();
                    }
                    return List.of("    mov r0, #" + val);
                }
                case FLOAT -> {
                    //todo
                    throw new RuntimeException("Todo");
                }
                case LONG -> {
                    long val = 0;
                    if (literal instanceof IntLiteral(Integer value)) {
                        val = value.longValue();
                    } else if (literal instanceof LongLiteral(Long value)) {
                        val = value;
                    } else if (literal instanceof FloatLiteral(Float value)) {
                        val = value.longValue();
                    }
                    int low = (int) val;
                    int high = (int) (val >>> 32);
                    String lowHex = "0x" + Integer.toHexString(low);
                    String highHex = "0x" + Integer.toHexString(high);
                    return List.of("    ldr r0, =" + lowHex, "    ldr r1, =" + highHex);
                }
            }
        } else if (expression instanceof Variable variable) {
            switch (allType) {
                case INT -> {
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
                }
                case FLOAT -> {
                    //todo
                    throw new RuntimeException("Todo");
                }
                case LONG -> {
                    Integer slot = localsMap.get(variable.name());
                    if (slot != null) {
                        instructions.add("  ldr r0, [fp, #-" + slot + "]");
                        instructions.add("  ldr r1, [fp, #-" + (slot + 4) + "]");
                        return instructions;
                    } else {
                        Integer posOff = paramStackMap.get(variable.name());
                        if (posOff == null) {
                            throw new IllegalStateException("Unknown variable or param: " + variable.name());
                        }
                        instructions.add("  ldr r0, [fp, #" + posOff + "]");
                        instructions.add("  ldr r1, [fp, #" + (posOff + 4) + "]");
                        return instructions;
                    }
                }
            }
        } else if (expression instanceof BinaryExpression binaryExpression) {
            switch (allType) {
                case INT -> {
                    instructions.addAll(generateExpression(binaryExpression.left(), localsMap, paramStackMap, allType));
                    instructions.add("  push {r0}");
                    instructions.addAll(generateExpression(binaryExpression.right(), localsMap, paramStackMap, allType));
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
                }
                case FLOAT -> {
                    //todo
                    throw new RuntimeException("Todo");
                }
                case LONG -> {
                    instructions.addAll(generateExpression(binaryExpression.left(), localsMap, paramStackMap, allType));
                    instructions.add("  push {r0, r1}");
                    instructions.addAll(generateExpression(binaryExpression.right(), localsMap, paramStackMap, allType));
                    instructions.add("  pop {r2, r3}");
                    switch (binaryExpression.op()) {
                        case PLUS -> {
                            instructions.add("  adds r0, r0, r2");
                            instructions.add("  adc r1, r1, r3");
                        }
                        case MINUS -> {
                            instructions.add("  subs r0, r0, r2");
                            instructions.add("  sbc r1, r1, r3");
                        }
                        case MUL -> {
                            instructions.add("  umull r0, r1, r0, r2");
                        }
                        case DIV -> {
                            //todo IDK, there is no div with long in arm7 spec
                            instructions.add("  mov r2, r0");
                            instructions.add("  mov r3, r1");
                            instructions.add("  bl __aeabi_ldivmod");
                        }
                    }
                    return instructions;
                }
            }
        }
        throw new IllegalStateException("Unexpected expression: " + expression);
    }

    private int getTypeSize(PrimitiveType type) {
        return switch (type) {
            case INT, FLOAT -> 4;
            case LONG -> 8;
            default -> throw new IllegalStateException("Unexpected value: " + type);
        };
    }

}
