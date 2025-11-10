package com.therepanic;

import com.therepanic.expression.*;
import com.therepanic.statement.FunctionStatement;
import com.therepanic.type.PointerType;
import com.therepanic.type.PrimitiveType;
import com.therepanic.type.Type;

import java.util.Map;

public final class TypeResolver {

    public static PrimitiveType inferType(Expression expr, Map<String, Type> localsTypes, Map<String, FunctionStatement> functions) {
        if (expr == null) return null;
        PrimitiveType result;
        if (expr instanceof IntLiteral) {
            result = PrimitiveType.INT;
        } else if (expr instanceof LongLiteral) {
            result = PrimitiveType.LONG;
        } else if (expr instanceof FloatLiteral) {
            result = PrimitiveType.FLOAT;
        } else if (expr instanceof Variable v) {
            Type type = localsTypes.get(v.name());
            if (type instanceof PrimitiveType primitive) {
                result = primitive;
            } else {
                result = ((PointerType) type).baseType();
            }
            if (result == null) {
                throw new IllegalStateException("Unexpected variable: " + v.name());
            }
        } else if (expr instanceof FunctionCallExpression fc) {
            Type ret = functions.get(fc.name()).returnType();
            if (ret == null) {
                throw new IllegalStateException("Unexpected function: " + fc.name());
            }
            for (Expression expression : fc.args()) {
                inferType(expression, localsTypes, functions);
            }
            if (ret instanceof PrimitiveType primitive) {
                result = primitive;
            } else {
                result = ((PointerType) ret).baseType();
            }
        } else if (expr instanceof BinaryExpression be) {
            Type lt = inferType(be.left(), localsTypes, functions);
            Type rt = inferType(be.right(), localsTypes, functions);
            result = promote(lt, rt);
        } else if (expr instanceof UnaryMinusExpression um) {
            result = inferType(um.inner(), localsTypes, functions);
        } else if (expr instanceof DerefExpression de) {
            result = inferType(de.inner(), localsTypes, functions);
        } else {
            throw new IllegalStateException("Unhandled expression type in inferType: " + expr);
        }
        return result;
    }

    private static PrimitiveType promote(Type a, Type b) {
        if (a == PrimitiveType.LONG || b == PrimitiveType.LONG) return PrimitiveType.LONG;
        if (a == PrimitiveType.FLOAT || b == PrimitiveType.FLOAT) return PrimitiveType.FLOAT;
        return PrimitiveType.INT;
    }

}
