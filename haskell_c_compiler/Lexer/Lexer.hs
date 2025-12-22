module Lexer where

import Token
import Data.Char

-- toy lexer (no comments support now)

lex :: Pos -> String -> [Token]
lex p = go p
    where
        go :: Pos -> String -> [Token]
        go p [] = [TokEOF p]
        go p (c:cs)
            | c == '\n' = go (Pos { line = line p + 1, col = 1} ) cs
            | isSpace c = go (p { col = col p + 1 }) cs
            | isAlpha c = lexIdent p (c:cs)
            | isDigit c = lexDig p (c:cs)
            | c == '+' = TokPlus p : go p{ col = col p + 1 } cs
            | c == '-' = TokMinus p : go p{ col = col p + 1 } cs
            | c == '*' = TokStar p : go p{ col = col p + 1 } cs
            | c == '/' = TokSlash p : go p{ col = col p + 1 } cs
            | c == '(' = TokLParen p : go p{ col = col p + 1 } cs
            | c == ')' = TokRParen p : go p{ col = col p + 1 } cs
            | c == ';' = TokSemicolon p : go p{col = col p + 1} cs
            | otherwise = error ("Unexpected character '" ++ [c] ++ "' at " ++ show p)

lexDig :: Pos -> String -> [Token]
lexDig p s =
    let
        (f, s) = span isDigit s
    in
        TokNum f p : Lexer.lex p{ col = col p + length f } s

lexIdent :: Pos -> String -> [Token]
lexIdent p s =
    let
        (f, s) = span isAlphaNum s
    in
        TokIdent f p : Lexer.lex p{ col = col p + length f } s
