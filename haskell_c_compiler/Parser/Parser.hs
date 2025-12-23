module Parser.Parser where

import AST.Expression
import AST.Lit
import AST.Statement
import Lexer.Token

-- parseAll :: [Token] -> [Statement]

parseExpr :: [Token] -> (Expression, [Token])
--todo
parseExpr t = (Var "d", [])

parseLiteral :: [Token] -> (Expression, [Token])
parseLiteral (TokNum val _ :ts) = (Literal (LNum val), ts)
parseLiteral _ = error "Expected numeric literal"

parseIdent :: [Token] -> (Expression, [Token])
parseIdent (TokIdent val _ : ts) = (Var val, ts)
parseIdent _ = error "Expected variable"

parsePrimary :: [Token] -> (Expression, [Token])
parsePrimary (TokLParen _ : ts) =
    let
        (a, rest) = parseExpr ts
    in
        case rest of
            TokRParen _ : bx -> (a, bx)
            _ -> error "Expected ')'"

parsePostfix :: Expression -> [Token] -> (Expression, [Token])
parsePostfix e (TokLParen t:ts) =
    let
        parseArgs :: [Token] -> ([Expression], [Token])
        parseArgs (TokRParen _ : rest) =
            ([], rest)
        parseArgs tokens =
            let
                (arg, rest) = parseExpr tokens
            in
                case rest of
                    TokComma _ : rest' ->
                        let
                            (args, rest'') = parseArgs rest'
                        in
                            (arg:args, rest'')
                    TokRParen _ : rest' -> 
                        ([arg], rest')
                    _ -> 
                        error "Expected ',' or ')'"
        (args, rest) = parseArgs ts
        callExpr = Call e args
    in
        parsePostfix callExpr rest
