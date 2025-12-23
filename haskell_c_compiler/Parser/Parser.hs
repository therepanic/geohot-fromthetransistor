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
            _ -> error "Expected ) rparen"
