module Parser.Parser where

import qualified AST.UnaryOp as U
import qualified AST.Statement as F

import AST.Expression
import AST.Lit
import AST.Type
import AST.Statement
import AST.Operator
import AST.VarDecl
import AST.Program
import Lexer.Token

-- =============================
-- Program parsing
-- =============================

parseProgram :: [Token] -> Program
parseProgram (TokEOF _ : _) = []
parseProgram toks =
    let
        (stmt, rest) = parseStatement toks
    in
        stmt : parseProgram rest

-- =============================
-- Statements parsing
-- =============================

parseStatement :: [Token] -> (Statement, [Token])
parseStatement toks@(TokIdent "if" t : ts) = parseIf toks
parseStatement toks@(TokIdent "while" t : ts) = parseWhile toks
parseStatement toks@(TokIdent "return" t : ts) = parseReturn toks
parseStatement (t:ts) =
    let
        isType = isTypeName t
    in
        if isType 
            then 
                let
                    (typ, rest) = parseType (t:ts)
                in
                    case rest of
                        (TokIdent name _ : TokLParen _ : rest') -> parseFunction (t:ts)
                        _ -> parseVarDecl (t:ts)
            else
                let
                    -- todo evaluates the expression twice before '='
                    (e, rest) = parseExpr (t:ts)
                in
                    case rest of
                        TokAssign _ : rest' -> parseAssign (t:ts)
                        _ -> parseExprStatement (t:ts)
parseStatement [] = error "Unexpected end of input"

-- Statement function parsing
parseParamList :: [Token] -> ([VarDecl], [Token])
parseParamList (TokRParen _ : rest) = ([], rest)
parseParamList tokens =
    let
        (ty, rest1) = parseType tokens
    in
        case rest1 of
            TokIdent name _ : rest2 ->
                let
                    param = VarDecl name ty Nothing
                in
                    case rest2 of
                        TokComma _ : rest3 ->
                            let
                                (params, rest4) = parseParamList rest3
                            in
                                (param : params, rest4)
                        TokRParen _ : rest3 -> ([param], rest3)
                        _ -> error "Expected ',' or ')' after parameter"
            _ -> error "Expected parameter name after type"
parseFunction :: [Token] -> (Statement, [Token])
parseFunction tokens =
    let
        (ty, rest1) = parseType tokens
    in
        case rest1 of
            TokIdent name _ : TokLParen _ : rest2 ->
                let
                    (v, rest3) = parseParamList rest2
                    params = [(pname, pty) | VarDecl pname pty _ <- v]
                    (v1, rest4) = parseBlock rest3
                in
                    (Function ty name params v1, rest4)
            _ -> error "Expected function name and '(' after type"

-- Statement assign parsing
parseAssign :: [Token] -> (Statement, [Token])
parseAssign ts =
    let
        (e, rest) = parseExpr ts
    in
        case rest of
            TokAssign _ : rest' ->
                let
                    (e', rest'') = parseExpr rest'
                in
                    case rest'' of
                        TokSemicolon _ : rest''' ->
                            (Assign e e', rest''')
                        _ -> error "Expected ';' after assignment"
            _ -> error "Expected '=' after expression"

-- Statement var declaration parsing
parseVarDecl :: [Token] -> (Statement, [Token])
parseVarDecl ts =
    let
        (ty, rest1) = parseType ts
    in
        case rest1 of
            TokIdent name _ : rest2 ->
                case rest2 of
                    TokSemicolon _ : rest3 ->
                        (VarDeclStmt (VarDecl name ty Nothing), rest3)

                    TokAssign _ : rest3 ->
                        let
                            (expr, rest4) = parseExpr rest3
                        in
                            case rest4 of
                                TokSemicolon _ : rest5 ->
                                    (VarDeclStmt (VarDecl name ty (Just expr)), rest5)
                                _ ->
                                    error "Expected ';' after initializer"

                    _ ->
                        error "Expected ';' or '=' after variable name"

            _ ->
                error "Expected variable name"

-- Statement if parsing
parseIf :: [Token] -> (Statement, [Token])
parseIf (TokIdent "if" _ : TokLParen _ : ts) =
    let
        (cond, rest1) = parseExpr ts
    in
        case rest1 of
            TokRParen _ : rest2 ->
                let
                    (ifBody, rest3) = parseBlock rest2
                in
                    case rest3 of
                        TokIdent "else" _ : rest4 ->
                            let
                                (elseBody, rest5) = parseBlock rest4
                            in
                                (If cond ifBody elseBody, rest5)

                        _ -> (If cond ifBody [], rest3)
            _ -> error "expected ')'"
parseIf _ = error "Expected 'if'"

-- Statement while parsing
parseWhile :: [Token] -> (Statement, [Token])
parseWhile (TokIdent "while" _ : TokLParen _ : ts) =
    let
        (cond, rest1) = parseExpr ts
    in
        case rest1 of
            TokRParen _ : rest2 ->
                let
                    (body, rest3) = parseBlock rest2
                in
                    (While cond body, rest3)
            _ -> error "Expected ')'"
parseWhile _ = error "Expected 'while'"

-- Statement return parsing
parseReturn :: [Token] -> (Statement, [Token])
parseReturn (TokIdent "return" _ : TokSemicolon _ : ts) = (Return Nothing, ts)
parseReturn (TokIdent "return" _ : ts) =
    let
        (expr, rest) = parseExpr ts
    in
        case rest of
            TokSemicolon _ : rest1 -> (Return (Just expr), rest1)
            _ -> error "Expected ';'"

-- Statement block parsing
parseBlock :: [Token] -> ([Statement], [Token])
parseBlock (TokLBrace _ : ts) = go ts
    where
        go (TokRBrace _ : rest) = ([], rest)
        go tokens =
            let
                (stmt, rest) = parseStatement tokens
                (stmts, rest') = go rest
            in
                (stmt : stmts, rest')

-- Statement Expression parsing
parseExprStatement :: [Token] -> (Statement, [Token])
parseExprStatement ts =
    let
        (expr, rest) = parseExpr ts
    in
        case rest of
            TokSemicolon _ : rest' ->
                (ExprStmt expr, rest')
            _ -> error "Expected ';'"

-- =============================
-- Expressions parsing
-- =============================
parseExpr :: [Token] -> (Expression, [Token])
parseExpr = parseEquality

-- Expression Equality parsing
parseEquality :: [Token] -> (Expression, [Token])
parseEquality ts =
    let
        (left, rest) = parseRelational ts
    in
        parseEquality' left rest

parseEquality' :: Expression -> [Token] -> (Expression, [Token])
parseEquality' left (TokEqEq _ : ts) =
    let
        (right, rest) = parseRelational ts
    in
        parseEquality' (Binary Eq left right) rest
parseEquality' left (TokNeq _ : ts) =
    let
        (right, rest) = parseRelational ts
    in
        parseEquality' (Binary Neq left right) rest
parseEquality' left ts = (left, ts)

-- Expression Relational parsing
parseRelational :: [Token] -> (Expression, [Token])
parseRelational ts =
    let
        (left, rest) = parseAdditive ts
    in
        parseRelational' left rest

parseRelational' :: Expression -> [Token] -> (Expression, [Token])
parseRelational' left (TokLt _  : ts)  =
    let
        (right, rest) = parseAdditive ts
    in
        parseRelational' (Binary Lt left right) rest
parseRelational' left (TokLte _ : ts)  =
    let
        (right, rest) = parseAdditive ts
    in
        parseRelational' (Binary Lte left right) rest
parseRelational' left (TokGt _  : ts)  =
    let
        (right, rest) = parseAdditive ts
    in
        parseRelational' (Binary Gt left right) rest
parseRelational' left (TokGte _ : ts)  =
    let
        (right, rest) = parseAdditive ts
    in
        parseRelational' (Binary Gte left right) rest
parseRelational' left ts = (left, ts)

-- Expression Additive parsing
parseAdditive :: [Token] -> (Expression, [Token])
parseAdditive ts =
    let
        (left, rest) = parseMultiplicative ts
    in
        parseAdditive' left rest
parseAdditive' :: Expression -> [Token] -> (Expression, [Token])
parseAdditive' left (TokPlus _ : ts) =
    let
        (right, rest) = parseMultiplicative ts
    in
        parseAdditive' (Binary Plus left right) rest
parseAdditive' left (TokMinus _ : ts) =
    let
        (right, rest) = parseMultiplicative ts
    in
        parseAdditive' (Binary Minus left right) rest
parseAdditive' left ts = (left, ts)

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
parseUnary :: [Token] -> (Expression, [Token])
parseUnary (TokLParen t : ts) =
    case ts of
        tok : _ | isTypeName tok ->
            let
                (ty, rest1) = parseType ts
            in
                case rest1 of
                    TokRParen _ : rest2 ->
                        let
                            (expr, rest3) = parseUnary rest2
                        in
                            (Cast ty expr, rest3)
                    _ -> error "expected ')'"
        _ ->
            parsePostfix (TokLParen t : ts)
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
parseUnary (TokNot _ : ts) =
    let
        (e, tokens) = parseUnary ts
    in
        (UnaryOp U.Not e, tokens)
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
        parseArgs (TokRParen _ : rest) = ([], rest)
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

-- =============================
-- Helpers
-- =============================

-- Check if it type name
isTypeName :: Token -> Bool
isTypeName (TokIdent "int" _) = True
isTypeName (TokIdent "long" _) = True
isTypeName (TokIdent "void" _) = True
isTypeName _ = False

-- Parse type
parseType :: [Token] -> (Type, [Token])
parseType (TokIdent "int" _ : ts) =
    (PrimitiveType Int, ts)
parseType (TokIdent "long" _ : ts) =
    (PrimitiveType Long, ts)
parseType (TokIdent "void" _ : ts) =
    (PrimitiveType Void, ts)
parseType _ = error "expected type"