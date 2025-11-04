package com.therepanic.lexer;

import com.therepanic.Token;
import com.therepanic.TokenType;

import java.util.ArrayList;
import java.util.List;

public class SimpleLexer implements Lexer {

    @Override
    public List<Token> tokenize(String source) {
        List<Token> tokens = new ArrayList<>();
        int length = source.length();
        for (int pos = 0; pos < length; pos++) {
            if (Character.isWhitespace(source.charAt(pos))) {
                continue;
            }
            if (Character.isLetter(source.charAt(pos))) {
                int start = pos;
                while (start < length && Character.isLetterOrDigit(source.charAt(start))) {
                    start++;
                }
                String word = source.substring(pos, start);
                TokenType tokenType = switch (word) {
                    case "if" -> TokenType.IF;
                    case "else" -> TokenType.ELSE;
                    case "while" -> TokenType.WHILE;
                    case "return" -> TokenType.RETURN;
                    case "struct" -> TokenType.STRUCT;
                    default -> TokenType.IDENTIFIER;
                };
                tokens.add(new Token(tokenType, word));
                pos = start - 1;
            } else if (Character.isDigit(source.charAt(pos))) {
                int start = pos;
                while (start < length && Character.isDigit(source.charAt(start))) {
                    start++;
                }
                String number = source.substring(pos, start);
                tokens.add(new Token(TokenType.NUMBER, number));
                pos = start - 1;
            } else {
                char current = source.charAt(pos);
                char next = (pos + 1 < length) ? source.charAt(pos + 1) : '\0';
                String symbols;
                if (current == '+' && next == '+') {
                    symbols = "++";
                    pos++;
                } else if (current == '-' && next == '-') {
                    symbols = "--";
                    pos++;
                } else if (current == '+' && next == '=') {
                    symbols = "+=";
                    pos++;
                } else if (current == '-' && next == '=') {
                    symbols = "-=";
                    pos++;
                } else if (current == '=' && next == '=') {
                    symbols = "==";
                    pos++;
                } else if (current == '!' && next == '=') {
                    symbols = "!=";
                    pos++;
                } else if (current == '>' && next == '=') {
                    symbols = ">=";
                    pos++;
                } else if (current == '<' && next == '=') {
                    symbols = "<=";
                    pos++;
                } else {
                    symbols = String.valueOf(current);
                }
                TokenType type = switch (symbols) {
                    case "+" -> TokenType.PLUS;
                    case "-" -> TokenType.MINUS;
                    case "=" -> TokenType.ASSIGN;
                    case ";" -> TokenType.SEMICOLON;
                    case "(" -> TokenType.LPAREN;
                    case ")" -> TokenType.RPAREN;
                    case "{" -> TokenType.LBRACE;
                    case "}" -> TokenType.RBRACE;
                    case "<" -> TokenType.LT;
                    case "," -> TokenType.COMMA;
                    case ">" -> TokenType.GT;
                    case ">=" -> TokenType.GTE;
                    case "<=" -> TokenType.LTE;
                    case "==" -> TokenType.EQ;
                    case "!=" -> TokenType.NEQ;
                    case "++" -> TokenType.INCREMENT;
                    case "--" -> TokenType.DECREMENT;
                    case "+=" -> TokenType.PLUS_ASSIGN;
                    case "-=" -> TokenType.MINUS_ASSIGN;
                    case "*" -> TokenType.MUL;
                    case "/" -> TokenType.DIV;
                    default -> throw new IllegalStateException("Unexpected character '" + source.charAt(pos) + "' at position " + pos);
                };
                tokens.add(new Token(type, symbols));
            }
        }
        return tokens;
    }

}
