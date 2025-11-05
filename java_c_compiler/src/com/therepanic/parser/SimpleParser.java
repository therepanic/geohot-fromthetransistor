package com.therepanic.parser;

import com.therepanic.Operator;
import com.therepanic.Token;
import com.therepanic.TokenType;
import com.therepanic.expression.BinaryExpression;
import com.therepanic.expression.Expression;
import com.therepanic.expression.IntLiteral;
import com.therepanic.expression.Variable;
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
            parameters.add(new VarDeclaration(tokens.get(this.pos++).text(), argLiteral));
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
                // int a = ...;
                if (isLiteral(tokens.get(this.pos).text())) {
                    String literalStr = tokens.get(this.pos++).text();
                    String variableName = tokens.get(this.pos++).text();
                    if (tokens.get(this.pos).type().equals(TokenType.SEMICOLON)) {
                        statements.add(new VarDeclaration(variableName, new IntLiteral(null)));
                        this.pos++;
                        continue;
                    }
                    expectToken(tokens, TokenType.ASSIGN);
                    this.pos++;
                    Expression value = parseSimpleExpression(tokens);
                    expectToken(tokens, TokenType.SEMICOLON);
                    this.pos++;
                    VarDeclaration variable = new VarDeclaration(variableName, value);
                    statements.add(variable);
                    continue;
                }
                String name = tokens.get(this.pos).text();
                this.pos++;
                TokenType type = tokens.get(this.pos).type();
                switch (type) {
                    case INCREMENT -> {
                        // a++
                        statements.add(new Assign(name, new BinaryExpression(new Variable(name), new IntLiteral(1), Operator.PLUS)));
                        expectToken(tokens, TokenType.SEMICOLON);
                        this.pos++;
                    }
                    case DECREMENT -> {
                        // a--
                        statements.add(new Assign(name, new BinaryExpression(new Variable(name), new IntLiteral(1), Operator.MINUS)));
                        expectToken(tokens, TokenType.SEMICOLON);
                        this.pos++;
                    }
                    case ASSIGN -> {
                        // a = <expr>;
                        this.pos++;
                        Expression value = parseSimpleExpression(tokens);
                        statements.add(new Assign(name, value));
                        expectToken(tokens, TokenType.SEMICOLON);
                        this.pos++;
                    }
                    case PLUS_ASSIGN -> {
                        // a += <expr>;
                        this.pos++;
                        Expression rhs = parseSimpleExpression(tokens);
                        statements.add(new Assign(name, new BinaryExpression(new Variable(name), rhs, Operator.PLUS)));
                        expectToken(tokens, TokenType.SEMICOLON);
                        this.pos++;
                    }
                    case MINUS_ASSIGN -> {
                        // a -= <expr>;
                        this.pos++;
                        Expression rhs = parseSimpleExpression(tokens);
                        statements.add(new Assign(name, new BinaryExpression(new Variable(name), rhs, Operator.MINUS)));
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
            }
        }
        return statements;
    }

    private Statement parseStructure(List<Token> tokens) {
        //todo
        return null;
    }

    private Expression convertLiteralStrToLiteral(String literal, Number value) {
        return switch (literal) {
            case "int" -> new IntLiteral(value == null ? null : value.intValue());
            default -> throw new IllegalArgumentException("Unknown literal " + literal);
        };
    }

    private boolean isLiteral(String value) {
        return switch (value) {
            case "int" -> true;
            default -> false;
        };
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
            } else break;
        }
        return left;
    }

    private Expression parseSimpleExpression(List<Token> tokens) {
        return parseEquality(tokens);
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
        Expression left = parsePrimary(tokens);
        while (this.pos < tokens.size()) {
            TokenType type = tokens.get(this.pos).type();
            Operator op = switch (type) {
                case MUL -> Operator.MUL;
                case DIV -> Operator.DIV;
                default -> null;
            };
            if (op != null) {
                this.pos++;
                Expression right = parsePrimary(tokens);
                left = new BinaryExpression(left, right, op);
            } else {
                break;
            }
        }
        return left;
    }

    private Expression parsePrimary(List<Token> tokens) {
        Token t = tokens.get(this.pos);
        this.pos++;
        if (t.type() == TokenType.NUMBER) {
            return new IntLiteral(Integer.parseInt(t.text()));
        } else if (t.type() == TokenType.IDENTIFIER) {
            return new Variable(t.text());
        } else if (t.type() == TokenType.LPAREN) {
            Expression expr = parseSimpleExpression(tokens);
            expectToken(tokens, TokenType.RPAREN);
            this.pos++;
            return expr;
        } else {
            throw new RuntimeException("Unexpected token in expression: " + t.type());
        }
    }

    private void expectToken(List<Token> tokens, TokenType expected) {
        if (!tokens.get(this.pos).type().equals(expected)) {
            throw new RuntimeException("Expected " + expected + " but got " + tokens.get(this.pos).type());
        }
    }

}
