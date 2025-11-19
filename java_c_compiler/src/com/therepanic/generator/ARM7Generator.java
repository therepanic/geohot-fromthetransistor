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

    private long labelCounter = 0;

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
        for (VarDeclaration p : functionStatement.parameters()) {
            int size = (p.type() instanceof PointerType) ? 4 : getTypeSize((PrimitiveType)p.type());
            if (size == 8 && offset % 8 != 0) offset += 4;
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
            int size;
            if (local.type() instanceof PointerType) {
                size = 4;
            } else {
                size = getTypeSize((PrimitiveType) local.type());
            }
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
        int runningPos = 8;
        for (int i = 0; i < argCount; i++) {
            VarDeclaration p = functionStatement.parameters().get(i);
            if (p.type().equals(PrimitiveType.INT)) {
                if (regIndex < 4) {
                    int slot = localsMap.get(p.name());
                    instructions.add("  str r" + regIndex + ", [fp, #-" + slot + "]");
                    regIndex++;
                } else {
                    paramStackMap.put(p.name(), runningPos);
                    runningPos += 4;
                    localsMap.remove(p.name());
                }
            } else if (p.type().equals(PrimitiveType.LONG)) {
                if (regIndex % 2 == 1) {
                    regIndex++;
                }
                if (regIndex < 3) {
                    regIndex += regIndex % 2;
                    int slot = localsMap.get(p.name());
                    instructions.add("  str r" + regIndex + ", [fp, #-" + slot + "]");
                    instructions.add("  str r" + (regIndex + 1) + ", [fp, #-" + (slot + 4) + "]");
                    regIndex += 2;
                } else {
                    paramStackMap.put(p.name(), runningPos);
                    runningPos += 8;
                    localsMap.remove(p.name());
                    regIndex = 4;
                }
            }
        }

        // local types searching
        Map<String, Type> localsTypes = getLocalsFromFunctionStatement(functionStatement);

        for (Statement statement : functionStatement.body()) {
            instructions.addAll(generateBodyStatement(functionStatement, statement, localsMap, paramStackMap, localsTypes));
        }
        instructions.add(functionStatement.name() + "_end:");
        if (offset > 4) {
            instructions.add("  add sp, sp, #" + (offset - 4));
        }
        instructions.add("  pop {fp, pc}");
        return instructions;
    }

    private List<String> generateBodyStatement(FunctionStatement functionStatement, Statement statement, Map<String, Integer> localsMap, Map<String, Integer> paramStackMap, Map<String, Type> localsTypes) {
        List<String> instructions = new ArrayList<>();
        if (statement instanceof VarDeclaration varDeclaration) {
            if (varDeclaration.value() != null) {
                PrimitiveType varDeclarationType = null;
                if (varDeclaration.type() instanceof PrimitiveType primitiveType) {
                    varDeclarationType = primitiveType;
                } else if (varDeclaration.type() instanceof PointerType pointerType) {
                    varDeclarationType = pointerType.baseType();
                }
                PrimitiveType allType = TypeResolver.inferType(varDeclaration.value(), localsTypes, this.functions);
                instructions.addAll(generateExpression(varDeclaration.value(), localsMap, paramStackMap, localsTypes, allType));
                switch (varDeclarationType) {
                    case INT -> {
                        if (localsMap.containsKey(varDeclaration.name())) {
                            instructions.add("  str r0, [fp, #-" + localsMap.get(varDeclaration.name()) + "]");
                        } else {
                            instructions.add("  str r0, [fp, #" + paramStackMap.get(varDeclaration.name()) + "]");
                        }
                    }
                    case LONG -> {
                        if (localsMap.containsKey(varDeclaration.name())) {
                            if (allType.equals(PrimitiveType.INT)) {
                                instructions.add("  asr r1, r0, #31");
                            }
                            instructions.add("  str r0, [fp, #-" + localsMap.get(varDeclaration.name()) + "]");
                            instructions.add("  str r1, [fp, #-" + (localsMap.get(varDeclaration.name()) + 4) + "]");
                        } else {
                            if (allType.equals(PrimitiveType.INT)) {
                                instructions.add("  asr r1, r0, #31");
                            }
                            instructions.add("  str r0, [fp, #" + paramStackMap.get(varDeclaration.name()) + "]");
                            instructions.add("  str r1, [fp, #" + (paramStackMap.get(varDeclaration.name()) + 4) + "]");
                        }
                    }
                    case FLOAT -> {
                        //todo: float
                    }
                }
            }
        } else if (statement instanceof Assign assign) {
            if (assign.lhs() instanceof Variable variable) {
                Type variableType = localsTypes.get(variable.name());
                PrimitiveType primitiveVariableType = null;
                if (variableType instanceof PrimitiveType primitiveType) {
                    primitiveVariableType = primitiveType;
                } else if (variableType instanceof PointerType pointerType) {
                    primitiveVariableType = pointerType.baseType();
                }
                PrimitiveType allType = TypeResolver.inferType(assign.rhs(), localsTypes, this.functions);
                instructions.addAll(generateExpression(assign.rhs(), localsMap, paramStackMap, localsTypes, allType));
                switch (primitiveVariableType) {
                    case INT -> {
                        if (localsMap.containsKey(variable.name())) {
                            instructions.add("  str r0, [fp, #-" + localsMap.get(variable.name()) + "]");
                        } else {
                            instructions.add("  str r0, [fp, #" + paramStackMap.get(variable.name()) + "]");
                        }
                    }
                    case LONG -> {
                        if (localsMap.containsKey(variable.name())) {
                            if (allType.equals(PrimitiveType.INT)) {
                                instructions.add("  asr r1, r0, #31");
                            }
                            instructions.add("  str r0, [fp, #-" + localsMap.get(variable.name()) + "]");
                            instructions.add("  str r1, [fp, #-" + (localsMap.get(variable.name()) + 4) + "]");
                        } else {
                            if (allType.equals(PrimitiveType.INT)) {
                                instructions.add("  asr r1, r0, #31");
                            }
                            instructions.add("  str r0, [fp, #" + paramStackMap.get(variable.name()) + "]");
                            instructions.add("  str r1, [fp, #" + (paramStackMap.get(variable.name()) + 4) + "]");
                        }
                    }
                    case FLOAT -> {
                        //todo: float
                    }
                }
            } else if (assign.lhs() instanceof DerefExpression derefExpression) {
                PrimitiveType rhsType = TypeResolver.inferType(assign.rhs(), localsTypes, this.functions);
                PrimitiveType targetType = TypeResolver.inferType(derefExpression, localsTypes, this.functions);
                instructions.addAll(generateExpression(assign.rhs(), localsMap, paramStackMap, localsTypes, rhsType));
                if (rhsType == PrimitiveType.LONG) {
                    instructions.add("  push {r0, r1}");
                } else {
                    instructions.add("  push {r0}");
                }
                Expression inner = derefExpression.inner();
                if (inner instanceof Variable variable) {
                    Integer slot = localsMap.get(variable.name());
                    if (slot != null) {
                        instructions.add("  ldr r0, [fp, #-" + slot + "]");
                    } else {
                        Integer pos = paramStackMap.get(variable.name());
                        instructions.add("  ldr r0, [fp, #" + pos + "]");
                    }
                }
                if (targetType.equals(PrimitiveType.LONG)) {
                    if (rhsType.equals(PrimitiveType.LONG)) {
                        instructions.add("  pop {r2, r3}");
                        instructions.add("  str r2, [r0]");
                        instructions.add("  str r3, [r0, #4]");
                    } else {
                        instructions.add("  pop {r2, r3}");
                        instructions.add("  asr r3, r2, #31");
                        instructions.add("  str r2, [r0]");
                        instructions.add("  str r3, [r0, #4]");
                    }
                } else if (targetType.equals(PrimitiveType.INT)) {
                    if (rhsType.equals(PrimitiveType.LONG)) {
                        instructions.add("  pop {r2, r3}");
                        instructions.add("  str r2, [r0]");
                    } else {
                        instructions.add("  pop {r1}");
                        instructions.add("  str r1, [r0]");
                    }
                }
            }
        } else if (statement instanceof ReturnStatement returnStatement) {
            if (returnStatement.value() != null) {
                PrimitiveType funcStatementType = null;
                if (functionStatement.returnType() instanceof PrimitiveType primitiveType) {
                    funcStatementType = primitiveType;
                } else if (functionStatement.returnType() instanceof PointerType pointerType) {
                    funcStatementType = pointerType.baseType();
                }
                PrimitiveType allType = TypeResolver.inferType(returnStatement.value(), localsTypes, this.functions);
                instructions.addAll(generateExpression(returnStatement.value(), localsMap, paramStackMap, localsTypes, allType));
                switch (funcStatementType) {
                    case LONG -> {
                        if (allType.equals(PrimitiveType.INT)) {
                            instructions.add("  asr r1, r0, #31");
                        }
                    }
                    case FLOAT -> {
                        //todo: float
                    }
                }
            } else {
                instructions.add("  mov r0, #0");
                instructions.add("  mov r1, #0");
            }
            instructions.add("  b " + functionStatement.name() + "_end");
        } else if (statement instanceof IfStatement ifStatement) {
            PrimitiveType allType = TypeResolver.inferType(ifStatement.cond(), localsTypes, this.functions);
            instructions.addAll(generateExpression(ifStatement.cond(), localsMap, paramStackMap, localsTypes, allType));
            instructions.add("  cmp r0, #0");
            long currentLabel = this.labelCounter++;
            if (ifStatement.elseBranch() == null) {
                instructions.add("  beq L_end_" + currentLabel);
                for (Statement s : ifStatement.thenBranch()) {
                    instructions.addAll(generateBodyStatement(functionStatement, s, localsMap, paramStackMap, localsTypes));
                }
                instructions.add("  L_end_" + currentLabel + ":");
            } else {
                instructions.add("  beq L_else_" + currentLabel);
                for (Statement s : ifStatement.thenBranch()) {
                    instructions.addAll(generateBodyStatement(functionStatement, s, localsMap, paramStackMap, localsTypes));
                }
                instructions.add("  b L_end_" + currentLabel);
                instructions.add("  L_else_" + currentLabel + ":");
                for (Statement s : ifStatement.elseBranch()) {
                    instructions.addAll(generateBodyStatement(functionStatement, s, localsMap, paramStackMap, localsTypes));
                }
                instructions.add("  L_end_" + currentLabel + ":");
            }
        } else if (statement instanceof WhileStatement whileStatement) {
            PrimitiveType allType = TypeResolver.inferType(whileStatement.cond(), localsTypes, this.functions);
            long currentLabel = this.labelCounter++;
            instructions.add("  loop_start_" + currentLabel + ":");
            instructions.addAll(generateExpression(whileStatement.cond(), localsMap, paramStackMap, localsTypes, allType));
            instructions.add("  cmp r0, #0");
            instructions.add("  beq loop_end_" + currentLabel);
            for (Statement s : whileStatement.body()) {
                instructions.addAll(generateBodyStatement(functionStatement, s, localsMap, paramStackMap, localsTypes));
            }
            instructions.add("  b loop_start_" + currentLabel);
            instructions.add("  loop_end_" + currentLabel + ":");
        } else if (statement instanceof FunctionCallStatement functionCallStatement) {
            FunctionCallExpression funcExpression = new FunctionCallExpression(
                    functionCallStatement.name(),
                    functionCallStatement.args()
            );
            instructions.addAll(generateExpression(funcExpression, localsMap, paramStackMap, localsTypes, null));
        }
        return instructions;
    }

    private List<String> generateExpression(Expression expression, Map<String, Integer> localsMap,  Map<String, Integer> paramStackMap, Map<String, Type> localsTypes, PrimitiveType allType) {
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
                    String hex = "0x" + Integer.toHexString(val);
                    return List.of("    ldr r0, =" + hex);
                }
                case FLOAT -> {
                    //todo: float
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
                    //todo: float
                    throw new RuntimeException("Todo");
                }
                case LONG -> {
                    Type realType = localsTypes.get(variable.name());
                    boolean isActuallyInt = realType instanceof PrimitiveType p && p == PrimitiveType.INT;
                    Integer slot = localsMap.get(variable.name());
                    if (slot != null) {
                        instructions.add("  ldr r0, [fp, #-" + slot + "]");
                        if (isActuallyInt) {
                            instructions.add("  asr r1, r0, #31");
                        } else {
                            instructions.add("  ldr r1, [fp, #-" + (slot + 4) + "]");
                        }
                        return instructions;
                    } else {
                        Integer posOff = paramStackMap.get(variable.name());
                        if (posOff == null) {
                            throw new IllegalStateException("Unknown variable or param: " + variable.name());
                        }
                        instructions.add("  ldr r0, [fp, #" + posOff + "]");
                        if (isActuallyInt) {
                            instructions.add("  asr r1, r0, #31");
                        } else {
                            instructions.add("  ldr r1, [fp, #" + (posOff + 4) + "]");
                        }
                        return instructions;
                    }
                }
            }
        } else if (expression instanceof BinaryExpression binaryExpression) {
            switch (allType) {
                case INT -> {
                    instructions.addAll(generateExpression(binaryExpression.left(), localsMap, paramStackMap, localsTypes, allType));
                    instructions.add("  push {r0}");
                    instructions.addAll(generateExpression(binaryExpression.right(), localsMap, paramStackMap, localsTypes, allType));
                    instructions.add("  pop {r1}");
                    switch (binaryExpression.op()) {
                        case PLUS -> instructions.add("  add"  + " r0, r1, r0");
                        case MINUS -> instructions.add("  sub"  + " r0, r1, r0");
                        case MUL -> instructions.add("  mul"  + " r0, r1, r0");
                        case DIV -> instructions.add("  div"  + " r0, r1, r0");
                        case GT -> {
                            instructions.add("  cmp"  + " r1, r0");
                            instructions.add("  mov"  + " r0, #0");
                            instructions.add("  movgt"  + " r0, #1");
                        }
                        case LT -> {
                            instructions.add("  cmp"  + " r1, r0");
                            instructions.add("  mov"  + " r0, #0");
                            instructions.add("  movlt"  + " r0, #1");
                        }
                        case GTE -> {
                            instructions.add("  cmp"  + " r1, r0");
                            instructions.add("  mov"  + " r0, #0");
                            instructions.add("  movgte"  + " r0, #1");
                        }
                        case LTE -> {
                            instructions.add("  cmp"  + " r1, r0");
                            instructions.add("  mov"  + " r0, #0");
                            instructions.add("  movlte"  + " r0, #1");
                        }
                        case EQ -> {
                            instructions.add("  cmp"  + " r1, r0");
                            instructions.add("  mov"  + " r0, #0");
                            instructions.add("  moveq"  + " r0, #1");
                        }
                        case NEQ -> {
                            instructions.add("  cmp"  + " r1, r0");
                            instructions.add("  mov"  + " r0, #0");
                            instructions.add("  movneq"  + " r0, #1");
                        }
                        default -> throw new IllegalStateException("Unexpected operation: " + binaryExpression.op());
                    };
                    return instructions;
                }
                case FLOAT -> {
                    //todo: float
                    throw new RuntimeException("Todo");
                }
                case LONG -> {
                    instructions.addAll(generateExpression(binaryExpression.left(), localsMap, paramStackMap, localsTypes, allType));
                    instructions.add("  push {r0, r1}");
                    instructions.addAll(generateExpression(binaryExpression.right(), localsMap, paramStackMap, localsTypes, allType));
                    instructions.add("  pop {r2, r3}");
                    switch (binaryExpression.op()) {
                        case PLUS -> {
                            instructions.add("  adds r0, r2, r0");
                            instructions.add("  adc r1, r3, r1");
                        }
                        case MINUS -> {
                            instructions.add("  subs r0, r2, r0");
                            instructions.add("  sbc r1, r3, r1");
                        }
                        case MUL -> {
                            //todo IDK, there is no mull 64x64  in arm7 spec
                            instructions.add("  push {r4}");
                            instructions.add("  mov  r4, r0");
                            instructions.add("  mov  r0, r2");
                            instructions.add("  mov  r2, r4");
                            instructions.add("  mov  r4, r1");
                            instructions.add("  mov  r1, r3");
                            instructions.add("  mov  r3, r4");
                            instructions.add("  pop  {r4}");
                            instructions.add("  bl __aeabi_lmul");
                        }
                        case DIV -> {
                            //todo IDK, there is no div with long in arm7 spec
                            instructions.add("  push {r4}");
                            instructions.add("  mov  r4, r0");
                            instructions.add("  mov  r0, r2");
                            instructions.add("  mov  r2, r4");
                            instructions.add("  mov  r4, r1");
                            instructions.add("  mov  r1, r3");
                            instructions.add("  mov  r3, r4");
                            instructions.add("  pop  {r4}");
                            instructions.add("  bl __aeabi_ldivmod");
                        }
                        case GT -> {
                            instructions.add("  cmp"  + " r3, r1");
                            instructions.add("  cmpeq"  + " r2, r0");
                            instructions.add("  mov"  + " r0, #0");
                            instructions.add("  movgt"  + " r0, #1");
                        }
                        case LT -> {
                            instructions.add("  cmp"  + " r3, r1");
                            instructions.add("  cmpeq"  + " r2, r0");
                            instructions.add("  mov"  + " r0, #0");
                            instructions.add("  movlt"  + " r0, #1");
                        }
                        case GTE -> {
                            instructions.add("  cmp"  + " r3, r1");
                            instructions.add("  cmpeq"  + " r2, r0");
                            instructions.add("  mov"  + " r0, #0");
                            instructions.add("  movgte"  + " r0, #1");
                        }
                        case LTE -> {
                            instructions.add("  cmp"  + " r3, r1");
                            instructions.add("  cmpeq"  + " r2, r0");
                            instructions.add("  mov"  + " r0, #0");
                            instructions.add("  movlte"  + " r0, #1");
                        }
                        case EQ -> {
                            instructions.add("  cmp"  + " r3, r1");
                            instructions.add("  cmpeq"  + " r2, r0");
                            instructions.add("  mov"  + " r0, #0");
                            instructions.add("  moveq"  + " r0, #1");
                        }
                        case NEQ -> {
                            instructions.add("  cmp"  + " r3, r1");
                            instructions.add("  cmpeq"  + " r2, r0");
                            instructions.add("  mov"  + " r0, #0");
                            instructions.add("  movneq"  + " r0, #1");
                        }
                    }
                    return instructions;
                }
            }
        } else if (expression instanceof UnaryMinusExpression unary) {
            instructions.addAll(generateExpression(unary.inner(), localsMap, paramStackMap, localsTypes, allType));
            switch (allType) {
                case INT -> {
                    instructions.add("  rsb r0, r0, #0");
                }
                case LONG -> {
                    instructions.add("  mvn r0, r0");
                    instructions.add("  mvn r1, r1");
                    instructions.add("  adds r0, r0, #1");
                    instructions.add("  adc r1, r1, #0");
                }
                case FLOAT -> {
                    //todo: float
                }
            }
            return instructions;
        } else if (expression instanceof FunctionCallExpression functionCall) {
            FunctionStatement currentFunctionStatement = this.functions.get(functionCall.name());
            if (currentFunctionStatement == null) {
                throw new IllegalStateException("There is no " + functionCall.name() + "function");
            }
            List<PrimitiveType> primitiveTypes = new ArrayList<>();
            int bytes = 0;
            for (Expression arg : functionCall.args()) {
                PrimitiveType type = TypeResolver.inferType(arg, localsTypes, this.functions);
                primitiveTypes.add(type);
                switch (type) {
                    case INT, FLOAT -> bytes += 4;
                    case LONG -> {
                        if (bytes % 8 != 0) {
                            bytes += 4;
                        }
                        bytes += 8;
                    }
                    case null, default -> throw new IllegalStateException("Unknown type: " + type);
                }
            }
            if (bytes % 8 != 0) {
                bytes += 4;
            }
            instructions.add("  push {r4, r5, r6, r7}");
            if (bytes > 0) {
                instructions.add("  sub sp, sp, #" + bytes);
            }
            int c = 0;
            int offset = 0;
            for (int i = 0; i < functionCall.args().size(); i++) {
                Expression arg = functionCall.args().get(i);
                PrimitiveType type = primitiveTypes.get(i);
                instructions.addAll(generateExpression(arg, localsMap, paramStackMap, localsTypes, type));
                switch (type) {
                    case INT -> {
                        if (c >= 4) {
                            instructions.add("  str r0, [sp, #" + offset + "]");
                            offset += 4;
                        } else {
                            instructions.add("  mov r" + (c + 4) + ", r0");
                            c++;
                        }
                    }
                    case LONG -> {
                        if (c >= 3) {
                            c = 4;
                            if (offset % 8 != 0) {
                                offset += 4;
                            }
                            instructions.add("  str r0, [sp, #" + offset + "]");
                            instructions.add("  str r1, [sp, #" + (offset + 4) + "]");
                            offset += 8;
                        } else {
                            if (c % 2 != 0) {
                                c++;
                            }
                            instructions.add("  mov r" + (c + 4) + ", r0");
                            instructions.add("  mov r" + (c + 4 + 1) + ", r1");
                            c += 2;
                        }
                    }
                    case FLOAT -> {
                        //todo: float
                    }
                }
            }
            for (int i = 0; i < Math.min(c, 4); i++) {
                instructions.add("  mov r" + i + ", r" + (i + 4));
            }
            instructions.add("  bl " + functionCall.name());
            if (bytes > 0) {
                instructions.add("  add sp, sp, #" + bytes);
            }
            instructions.add("  pop {r4, r5, r6, r7}");
            return instructions;
        } else if (expression instanceof AddressOfExpression addressOfExpression) {
            Expression inner = addressOfExpression.inner();
            if (inner instanceof Variable variable) {
                Integer slot = localsMap.get(variable.name());
                if (slot != null) {
                    instructions.add("  add r0, fp, #-" + slot);
                } else {
                    Integer pos = paramStackMap.get(variable.name());
                    instructions.add("  add r0, fp, #" + pos);
                }
            }
            return instructions;
        } else if (expression instanceof DerefExpression derefExpression) {
            Expression inner = derefExpression.inner();

            if (inner instanceof Variable variable) {
                Integer slot = localsMap.get(variable.name());
                if (slot != null) {
                    instructions.add("  ldr r0, [fp, #-" + slot + "]");
                } else {
                    Integer pos = paramStackMap.get(variable.name());
                    instructions.add("  ldr r0, [fp, #" + pos + "]");
                }
                PrimitiveType resultType = TypeResolver.inferType(derefExpression, localsTypes, this.functions);
                if (resultType.equals(PrimitiveType.LONG)) {
                    instructions.add("  ldr r1, [r0, #4]");
                    instructions.add("  ldr r0, [r0]");
                } else if (resultType.equals(PrimitiveType.INT)) {
                    instructions.add("  ldr r0, [r0]");
                }
            }
            return instructions;
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

}
