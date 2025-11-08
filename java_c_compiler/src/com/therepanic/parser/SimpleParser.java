package com.therepanic.parser;

import com.therepanic.Operator;
import com.therepanic.Token;
import com.therepanic.TokenType;
import com.therepanic.expression.*;
import com.therepanic.statement.*;

import java.util.ArrayList;
import java.util.List;

public class SimpleParser implements Parser {

    private int pos;

    @Override
    public List<Statement> parse(List<Token> tokens) {
        List<Statement> statements = new ArrayList<>();
        while (this.pos < tokens.size()) {
            statements.add(parseStatement(tokens));
        }
        this.pos = 0;
        return statements;
    }


    private Statement parseStatement(List<Token> tokens) {
        if (tokens.get(this.pos).type().equals(TokenType.STRUCT)) {
            //structure
            return parseStructure(tokens);
        } else {
            //function
            return parseFunction(tokens);
        }
    }

    private Statement parseFunction(List<Token> tokens) {
        Variable returnType = new Variable(tokens.get(this.pos++).text());
        String name = tokens.get(this.pos++).text();
        this.pos++;
        List<VarDeclaration> parameters = new ArrayList<>();
        while (!tokens.get(this.pos).type().equals(TokenType.RPAREN)) {
            if (tokens.get(this.pos).type().equals(TokenType.COMMA)) {
                this.pos++;
                continue;
            }
            Expression argLiteral = convertLiteralStrToLiteral(tokens.get(this.pos++).text(), null);
            parameters.add(new VarDeclaration(tokens.get(this.pos++).text(), argLiteral, null));
        }
        expectToken(tokens, TokenType.RPAREN);
        this.pos++;
        expectToken(tokens, TokenType.LBRACE);
        this.pos++;
        List<Statement> body = new ArrayList<>(parseBody(tokens));
        expectToken(tokens, TokenType.RBRACE);
        this.pos++;
        return new FunctionStatement(returnType, name, parameters, body);
    }

    private List<Statement> parseBody(List<Token> tokens) {
        List<Statement> statements = new ArrayList<>();
        while (this.pos < tokens.size() && !tokens.get(this.pos).type().equals(TokenType.RBRACE)) {
            // int a = ...;
            // a
            if (tokens.get(this.pos).type().equals(TokenType.IDENTIFIER)) {
                if (this.pos + 1 < tokens.size() && tokens.get(this.pos + 1).type().equals(TokenType.LPAREN)) {
                    String funcName = tokens.get(this.pos).text();
                    this.pos++;
                    expectToken(tokens, TokenType.LPAREN);
                    this.pos++;
                    List<Expression> args = new ArrayList<>();
                    while (!tokens.get(this.pos).type().equals(TokenType.RPAREN)) {
                        if (tokens.get(this.pos).type().equals(TokenType.COMMA)) {
                            this.pos++;
                            continue;
                        }
                        args.add(parseSimpleExpression(tokens));
                    }
                    expectToken(tokens, TokenType.RPAREN);
                    this.pos++;
                    expectToken(tokens, TokenType.SEMICOLON);
                    this.pos++;
                    statements.add(new FunctionCallStatement(funcName, args));
                    continue;
                }
                // int a = ...;
                if (isLiteral(tokens.get(this.pos).text())) {
                    String baseType = tokens.get(this.pos++).text();
                    int pointerDepth = 0;
                    while (tokens.get(this.pos).type().equals(TokenType.MUL)) {
                        pointerDepth++;
                        this.pos++;
                    }
                    String variableName = tokens.get(this.pos++).text();
                    PointerType type = new PointerType(baseType, pointerDepth);

                    Expression value = null;
                    if (tokens.get(this.pos).type().equals(TokenType.ASSIGN)) {
                        this.pos++;
                        value = parseSimpleExpression(tokens);
                    }
                    expectToken(tokens, TokenType.SEMICOLON);
                    this.pos++;
                    statements.add(new VarDeclaration(variableName, value, type));
                    continue;
                }

                String name = tokens.get(this.pos).text();
                this.pos++;
                TokenType type = tokens.get(this.pos).type();
                switch (type) {
                    case INCREMENT -> {
                        // a++
                        statements.add(new Assign(new Variable(name), new BinaryExpression(new Variable(name), new IntLiteral(1), Operator.PLUS)));
                        this.pos++;
                        expectToken(tokens, TokenType.SEMICOLON);
                        this.pos++;
                    }
                    case DECREMENT -> {
                        // a--
                        statements.add(new Assign(new Variable(name), new BinaryExpression(new Variable(name), new IntLiteral(1), Operator.MINUS)));
                        this.pos++;
                        expectToken(tokens, TokenType.SEMICOLON);
                        this.pos++;
                    }
                    case ASSIGN -> {
                        // a = <expr>;
                        this.pos++;
                        Expression value = parseSimpleExpression(tokens);
                        statements.add(new Assign(new Variable(name), value));
                        expectToken(tokens, TokenType.SEMICOLON);
                        this.pos++;
                    }
                    case PLUS_ASSIGN -> {
                        // a += <expr>;
                        this.pos++;
                        Expression rhs = parseSimpleExpression(tokens);
                        statements.add(new Assign(new Variable(name), new BinaryExpression(new Variable(name), rhs, Operator.PLUS)));
                        expectToken(tokens, TokenType.SEMICOLON);
                        this.pos++;
                    }
                    case MINUS_ASSIGN -> {
                        // a -= <expr>;
                        this.pos++;
                        Expression rhs = parseSimpleExpression(tokens);
                        statements.add(new Assign(new Variable(name), new BinaryExpression(new Variable(name), rhs, Operator.MINUS)));
                        expectToken(tokens, TokenType.SEMICOLON);
                        this.pos++;
                    }
                    default -> throw new RuntimeException("Unexpected token after identifier: " + type);
                }
            } else if (tokens.get(this.pos).type().equals(TokenType.WHILE)) {
                this.pos++;
                expectToken(tokens, TokenType.LPAREN);
                this.pos++;
                Expression cond = parseSimpleExpression(tokens);
                expectToken(tokens, TokenType.RPAREN);
                this.pos++;
                expectToken(tokens, TokenType.LBRACE);
                this.pos++;
                List<Statement> body = parseBody(tokens);
                expectToken(tokens, TokenType.RBRACE);
                this.pos++;
                statements.add(new WhileStatement(cond, body));
            } else if (tokens.get(this.pos).type().equals(TokenType.IF)) {
                this.pos++;
                expectToken(tokens, TokenType.LPAREN);
                this.pos++;
                Expression cond = parseSimpleExpression(tokens);
                expectToken(tokens, TokenType.RPAREN);
                this.pos++;
                expectToken(tokens, TokenType.LBRACE);
                this.pos++;
                List<Statement> thenBranch = parseBody(tokens);
                expectToken(tokens, TokenType.RBRACE);
                this.pos++;
                List<Statement> elseBranch = null;
                if (this.pos < tokens.size() && tokens.get(this.pos).type().equals(TokenType.ELSE)) {
                    this.pos++;
                    expectToken(tokens, TokenType.LBRACE);
                    this.pos++;
                    elseBranch = parseBody(tokens);
                    expectToken(tokens, TokenType.RBRACE);
                    this.pos++;
                }
                statements.add(new IfStatement(cond, thenBranch, elseBranch));
            } else if (tokens.get(this.pos).type().equals(TokenType.RETURN)) {
                this.pos++;
                Expression value = null;
                if (!tokens.get(this.pos).type().equals(TokenType.SEMICOLON)) {
                    value = parseSimpleExpression(tokens);
                }
                expectToken(tokens, TokenType.SEMICOLON);
                this.pos++;
                statements.add(new ReturnStatement(value));
            } else if (tokens.get(this.pos).type().equals(TokenType.MUL)
                    || tokens.get(this.pos).type().equals(TokenType.AND)
                    || tokens.get(this.pos).type().equals(TokenType.LPAREN)
                    || tokens.get(this.pos).type().equals(TokenType.NUMBER)
                    || tokens.get(this.pos).type().equals(TokenType.MINUS)) {
                Expression left = parseSimpleExpression(tokens);
                // like "*b = <expr>;"
                if (this.pos < tokens.size() && tokens.get(this.pos).type().equals(TokenType.ASSIGN)) {
                    this.pos++; // skip '='
                    Expression value = parseSimpleExpression(tokens);
                    statements.add(new Assign(left, value));
                    expectToken(tokens, TokenType.SEMICOLON);
                    this.pos++;
                } else {
                    expectToken(tokens, TokenType.SEMICOLON);
                    this.pos++;
                }
            }
        }
        return statements;
    }

    private Statement parseStructure(List<Token> tokens) {
        Token firstToken = tokens.get(this.pos++);
        List<VarDeclaration> fields = new ArrayList<>();
        String name = null;
        boolean typedef = false;
        if (firstToken.type().equals(TokenType.TYPEDEF)) {
            typedef = true;
            expectToken(tokens, TokenType.STRUCT);
            this.pos++;
            expectToken(tokens, TokenType.LBRACE);
        } else {
            // if "struct" first
            name = tokens.get(this.pos++).text();
            expectToken(tokens, TokenType.LBRACE);
        }
        this.pos++;
        while (!tokens.get(this.pos).type().equals(TokenType.RBRACE)) {
            if (isLiteral(tokens.get(this.pos).text())) {
                String type = tokens.get(this.pos++).text();
                int pointerDepth = 0;
                while (tokens.get(this.pos).type().equals(TokenType.MUL)) {
                    pointerDepth++;
                    this.pos++;
                }
                String fieldName = tokens.get(this.pos++).text();
                expectToken(tokens, TokenType.SEMICOLON);
                this.pos++;
                fields.add(new VarDeclaration(fieldName, null, new PointerType(type, pointerDepth)));
            } else {
                throw new RuntimeException("Unexpected token in struct fields: " + tokens.get(this.pos).type());
            }
        }
        expectToken(tokens, TokenType.RBRACE);
        this.pos++;
        if (typedef) {
            name = tokens.get(this.pos++).text();
        }
        expectToken(tokens, TokenType.SEMICOLON);
        this.pos++;
        return new StructStatement(name, fields, typedef);
    }

    private Expression convertLiteralStrToLiteral(String literal, Number value) {
        return switch (literal) {
            case "int" -> new IntLiteral(value == null ? null : value.intValue());
            case "long" -> new LongLiteral(value == null ? null : value.longValue());
            case "float" -> new FloatLiteral(value == null ? null : value.floatValue());
            default -> throw new IllegalStateException("Unknown literal " + literal);
        };
    }

    private boolean isLiteral(String value) {
        return switch (value) {
            case "int" -> true;
            case "long" -> true;
            case "float" -> true;
            default -> false;
        };
    }

    private Expression parseSimpleExpression(List<Token> tokens) {
        return parseEquality(tokens);
    }

    private Expression parseEquality(List<Token> tokens) {
        Expression left = parseComparison(tokens);
        while (this.pos < tokens.size()) {
            TokenType type = tokens.get(this.pos).type();
            Operator op = switch (type) {
                case EQ -> Operator.EQ;
                case NEQ -> Operator.NEQ;
                default -> null;
            };
            if (op != null) {
                this.pos++;
                Expression right = parseComparison(tokens);
                left = new BinaryExpression(left, right, op);
            } else {
                break;
            }
        }
        return left;
    }

    private Expression parseComparison(List<Token> tokens) {
        Expression left = parseTerm(tokens);
        while (this.pos < tokens.size()) {
            TokenType type = tokens.get(this.pos).type();
            Operator op = switch (type) {
                case LT -> Operator.LT;
                case LTE -> Operator.LTE;
                case GT -> Operator.GT;
                case GTE -> Operator.GTE;
                default -> null;
            };
            if (op != null) {
                this.pos++;
                Expression right = parseTerm(tokens);
                left = new BinaryExpression(left, right, op);
            } else {
                break;
            }
        }
        return left;
    }


    private Expression parseTerm(List<Token> tokens) {
        Expression left = parseFactor(tokens);
        while (this.pos < tokens.size()) {
            TokenType type = tokens.get(this.pos).type();
            Operator op = switch (type) {
                case PLUS -> Operator.PLUS;
                case MINUS -> Operator.MINUS;
                default -> null;
            };
            if (op != null) {
                this.pos++;
                Expression right = parseFactor(tokens);
                left = new BinaryExpression(left, right, op);
            } else {
                break;
            }
        }
        return left;
    }


    private Expression parseFactor(List<Token> tokens) {
        Expression left = parseUnary(tokens);
        while (this.pos < tokens.size()) {
            TokenType type = tokens.get(this.pos).type();
            Operator op = switch (type) {
                case MUL -> Operator.MUL;
                case DIV -> Operator.DIV;
                default -> null;
            };
            if (op != null) {
                this.pos++;
                Expression right = parseUnary(tokens);
                left = new BinaryExpression(left, right, op);
            } else {
                break;
            }
        }
        return left;
    }

    private Expression parseUnary(List<Token> tokens) {
        if (this.pos >= tokens.size()) return null;
        TokenType type = tokens.get(this.pos).type();
        if (type == TokenType.MUL) {
            this.pos++;
            return new DerefExpression(parseUnary(tokens));
        } else if (type == TokenType.AND) {
            this.pos++;
            return new AddressOfExpression(parseUnary(tokens));
        } else if (type == TokenType.MINUS) {
            this.pos++;
            return new UnaryMinusExpression(parseUnary(tokens));
        }
        return parsePrimary(tokens);
    }

    private Expression parsePrimary(List<Token> tokens) {
        Token t = tokens.get(this.pos);
        this.pos++;
        if (t.type() == TokenType.NUMBER) {
            String text = t.text();
            if (text.endsWith("L") || text.endsWith("l")) {
                long val = Long.parseLong(text.substring(0, text.length() - 1));
                return new LongLiteral(val);
            } else if (text.endsWith("F") || text.endsWith("f") || text.contains(".")) {
                float val = Float.parseFloat(text.replaceAll("[fF]", ""));
                return new FloatLiteral(val);
            } else {
                int val = Integer.parseInt(text);
                return new IntLiteral(val);
            }
        } else if (t.type() == TokenType.IDENTIFIER) {
            if (this.pos < tokens.size() && tokens.get(this.pos).type().equals(TokenType.LPAREN)) {
                this.pos++;
                List<Expression> args = new ArrayList<>();
                while (!tokens.get(this.pos).type().equals(TokenType.RPAREN)) {
                    if (tokens.get(this.pos).type().equals(TokenType.COMMA)) {
                        this.pos++;
                        continue;
                    }
                    args.add(parseSimpleExpression(tokens));
                }
                expectToken(tokens, TokenType.RPAREN);
                this.pos++;
                return new FunctionCallExpression(t.text(), args);
            }
            return new Variable(t.text());
        } else if (t.type() == TokenType.LPAREN) {
            Expression expr = parseSimpleExpression(tokens);
            expectToken(tokens, TokenType.RPAREN);
            this.pos++;
            return expr;
        } else {
            throw new IllegalStateException("Unexpected token in expression: " + t.type());
        }
    }

    private void expectToken(List<Token> tokens, TokenType expected) {
        if (!tokens.get(this.pos).type().equals(expected)) {
            throw new IllegalStateException("Expected " + expected + " but got " + tokens.get(this.pos).type());
        }
    }

}
