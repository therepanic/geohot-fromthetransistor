module Lexer.Lexer where

import Lexer.Token
import Data.Char

-- toy lexer (no comments support now)

lex :: Pos -> String -> [Token]
lex p = go p
    where
        go :: Pos -> String -> [Token]
        go p [] = [TokEOF p]
        go p s@(c:c1:cs)
            | c == '<' && c1 == '=' = TokLte p : go p{ col = col p + 2 } cs
            | c == '>' && c1 == '=' = TokGte p : go p{ col = col p + 2 } cs
            | c == '=' && c1 == '=' = TokEqEq p : go p{ col = col p + 2 } cs
            | c == '!' && c1 == '=' = TokNeq p : go p{ col = col p + 2 } cs
            | otherwise = go1 p s
        go1 :: Pos -> String -> [Token]
        go1 p [] = [TokEOF p]
        go1 p (c:cs)
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
            | c == '=' = TokAssign p : go p{ col = col p + 1 } cs
            | c == ';' = TokSemicolon p : go p{col = col p + 1} cs
            | c == ',' = TokSemicolon p : go p{col = col p + 1} cs
            | c == '<' = TokLt p : go p{ col = col p + 1 } cs
            | c == '>' = TokGt p : go p{ col = col p + 1 } cs
            | otherwise = error ("Unexpected character '" ++ [c] ++ "' at " ++ show p)

lexDig :: Pos -> String -> [Token]
lexDig p s =
    let
        (f, s) = span isDigit s
    in
        TokNum f p : Lexer.Lexer.lex p{ col = col p + length f } s

lexIdent :: Pos -> String -> [Token]
lexIdent p s =
    let
        (f, s) = span isAlphaNum s
    in
        TokIdent f p : Lexer.Lexer.lex p{ col = col p + length f } s
