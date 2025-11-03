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
                int start = pos;
                while (start < length && !Character.isLetterOrDigit(source.charAt(start))
                        && !Character.isWhitespace(source.charAt(start))) {
                    start++;
                }
                String symbols = source.substring(pos, start);
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
                    case ">" -> TokenType.GT;
                    case ">=" -> TokenType.GTE;
                    case "<=" -> TokenType.LTE;
                    case "==" -> TokenType.EQ;
                    case "!=" -> TokenType.NEQ;
                    default -> throw new IllegalStateException("Unexpected character '" + source.charAt(pos) + "' at position " + pos);
                };
                tokens.add(new Token(type, symbols));
                pos = start - 1;
            }
        }
        return tokens;
    }

}
