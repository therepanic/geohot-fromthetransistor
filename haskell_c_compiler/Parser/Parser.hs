module Parser.Parser where

import qualified AST.UnaryOp as U
import AST.Expression
import AST.Lit
import AST.Type
import AST.Statement
import AST.Operator
import Lexer.Token

-- EXPRESSIONS PARSING
parseExpr :: [Token] -> (Expression, [Token])
parseExpr = parseAdditive

-- Expression Additive parsing
parseAdditive :: [Token] -> (Expression, [Token])
parseAdditive ts =
    let
        (left, rest) = parseMultiplicative ts
    in
        parseAddictive' left rest
parseAdditive' :: Expression -> [Token] -> (Expression, [Token])
parseAdditive' left (TokPlus _ : ts) =
    let
        (right, rest) = parseMultiplicative ts
    in
        parseAddictive' (Binary Plus left right) rest
parseAddictive' left (TokMinus _ : ts) =
    let
        (right, rest) = parseMultiplicative ts
    in
        parseAddictive' (Binary Minus left right) rest
parseAddictive' left ts = (left, ts)

-- Expression Multiplication parsing
parseMultiplicative :: [Token] -> (Expression, [Token])
parseMultiplicative ts =
    let
        (left, rest) = parseUnary ts
    in
        parseMultiplicative' left rest
parseMultiplicative' :: Expression -> [Token] -> (Expression, [Token])
parseMultiplicative' left (TokStar _ : ts) =
    let
        (right, rest) = parseUnary ts
    in
        parseMultiplicative' (Binary Mul left right) rest
parseMultiplicative' left (TokSlash _ : ts) =
    let
        (right, rest) = parseUnary ts
    in
        parseMultiplicative' (Binary Div left right) rest
parseMultiplicative' left ts = (left, ts)

-- Expression Unary parsing
-- Check if it type name
isTypeName :: Token -> Bool
isTypeName (TokIdent "int" _) = True
isTypeName (TokIdent "long" _) = True
isTypeName _ = False
-- Parse type
parseType :: [Token] -> (Type, [Token])
parseType (TokIdent "int" _ : ts) =
    (PrimitiveType Int, ts)
parseType (TokIdent "long" _ : ts) =
    (PrimitiveType Long, ts)
parseType _ = error "expected type"

parseUnary :: [Token] -> (Expression, [Token])
parseUnary (TokLParen _ : ts) =
    case ts of
        TokIdent _ _ : _ ->
            let
                (ty, rest1) = parseType ts
            in
                case rest1 of
                    TokRParen _ : rest2 ->
                        let (expr, rest3) = parseUnary rest2
                        in (Cast ty expr, rest3)
                    _ -> error "expected ')'"
        _ ->
            parsePostfix (TokLParen undefined : ts)
parseUnary (TokMinus _ : ts) =
    let
        (e, tokens) = parseUnary ts
    in
        (UnaryOp U.Neg e, tokens)
parseUnary (TokPlus _ : ts) =
    let
        (e, tokens) = parseUnary ts
    in
        (UnaryOp U.Pos e, tokens)
parseUnary (TokStar _ : ts) =
    let
        (e, tokens) = parseUnary ts
    in
        (Deref e, tokens)
parseUnary (TokAmpersand _ : ts) =
    let
        (e, tokens) = parseUnary ts
    in
        (AddressOf e, tokens)
parseUnary ts = parsePostfix ts

-- Expression Postfix parsing
parsePostfix :: [Token] -> (Expression, [Token])
parsePostfix ts =
    let
        (expr, rest) = parsePrimary ts
    in
        parsePostfix' expr rest
parsePostfix' :: Expression -> [Token] -> (Expression, [Token])
parsePostfix' e (TokLParen t:ts) =
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
        parsePostfix' callExpr rest
parsePostfix' e ts = (e, ts)

-- Expression Primary parsing
parsePrimary :: [Token] -> (Expression, [Token])
parsePrimary (TokLParen _ : ts) =
    let
        (a, rest) = parseExpr ts
    in
        case rest of
            TokRParen _ : bx -> (a, bx)
            _ -> error "Expected ')'"
parsePrimary (TokIdent val _ : ts) = (Var val, ts)
parsePrimary (TokNum val _ : ts) = (Literal (LNum val), ts)