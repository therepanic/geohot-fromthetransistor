module Semantic.Check where

import qualified Data.Map.Strict as Map

import AST.Expression
import AST.Operator
import AST.Statement
import AST.VarDecl
import AST.Type
import AST.Lit
import Semantic.Types
import Semantic.Scope
import Semantic.Collect
import Data.List (foldl')

-- =============================
-- Program checking
-- =============================

checkProgram :: [Statement] -> TProgram
checkProgram stmts = TProgram (map (checkFunction ge) stmts)
    where
        ge = collectFunctions stmts

-- =============================
-- Functions checking
-- =============================

checkFunction :: GlobalEnv -> Statement -> TFunction
checkFunction ge (Function typ name args body) =
    let
        tbody = checkBlock ge typ [collect args] body
    in
        TFunction {tfReturnType=typ, tfName=name, tfParams=args, tfBody=tbody}
    where
        collect :: [(String, Type)] -> Map.Map String Type
        collect [] = Map.empty
        collect ((x, y):a) =
            let
                m = Map.insert x y (collect a)
            in
                m
checkFunction ge _ = error "Not function"

-- =============================
-- Statements checking
-- =============================

checkStatement :: GlobalEnv -> Type -> LocalEnv -> Statement -> (LocalEnv, TStatement)

-- Statement var declaration checking
checkStatement ge _ (l:le) (VarDeclStmt (VarDecl name typ init)) =
    if Map.member name l then error ("Variable with name " ++ name ++ " already exists")
    else let
        m = Map.insert name typ l
    in
        case init of
            Nothing -> (m:le, TVarDecl name typ Nothing)
            Just expr ->
                let
                    te = checkExpression ge (l:le) expr
                    te'  = coerceAssign typ te
                in
                    (m:le, TVarDecl name typ (Just te'))

-- Statement assign checking
checkStatement ge _ (l:le) (Assign lhs rhs) =
    let
        tl = checkExpression ge (l:le) lhs
    in
        case texprNode tl of
            TDeref _ ->
                let
                    tr  = checkExpression ge (l:le) rhs
                    tr' = coerceAssign (texprType tl) tr
                in
                    (l:le, TAssign tl tr')
            TVar _ ->
                let
                    tr  = checkExpression ge (l:le) rhs
                    tr' = coerceAssign (texprType tl) tr
                in
                    (l:le, TAssign tl tr')
            _ -> error "LHS of assignment is not an lvalue"

-- Statement expression checking
checkStatement ge _ le (ExprStmt e) = (le, TExprStmt (checkExpression ge le e))

-- Statement if checking
checkStatement ge t le (If cond th elze) =
    let
        condexpr = checkExpression ge le cond
        thenStmts = checkBlock ge t (Map.empty : le) th
        elseStmts = checkBlock ge t (Map.empty : le) elze
    in
        (le, TIf condexpr thenStmts elseStmts)

-- Statement while checking
checkStatement ge t le (While cond body) =
    let
        condexpr = checkExpression ge le cond
    in
        (le, TWhile condexpr (checkBlock ge t (Map.empty : le) body))

-- Statement return checking
checkStatement ge t le (Return (Just v)) =
    let
        te = checkExpression ge le v
        te' = coerceAssign t te
    in
        (le, TReturn (Just te'))

checkStatement ge t le (Return Nothing) =
    case t of
        PrimitiveType Void -> (le, TReturn Nothing)
        _ -> error "Return without value in non-void function"

checkBlock :: GlobalEnv -> Type -> LocalEnv -> [Statement] -> [TStatement]
checkBlock ge typ le stmts =
    reverse $ snd $ foldl' step (le, []) stmts
    where
        step (env, acc) s =
            let
                (env', ts) = checkStatement ge typ env s
            in
                (env', ts : acc)

-- =============================
-- Expressions checking
-- =============================

checkExpression :: GlobalEnv -> LocalEnv -> Expression -> TExpression

-- Expression var checking
checkExpression _ le (Var name) =
    let
        containsVar :: LocalEnv -> String -> TExpression
        containsVar [] name = error ("There is no variable with name " ++ name)
        containsVar (l:rest) name =
            case Map.lookup name l of
                Just v -> TExpression {texprType=v, texprNode=TVar name}
                Nothing -> containsVar rest name
    in
        containsVar le name

-- Expression literal checking
checkExpression _ le (Literal lit) = TExpression {texprType=PrimitiveType Int, texprNode=TLiteral lit}

-- Expression unary op checking
checkExpression ge le (UnaryOp s expr) =
    let
        texpr = checkExpression ge le expr
    in
        TExpression {texprType=texprType texpr, texprNode=TUnaryOp s texpr}

-- Expression binary checking
checkExpression ge le (Binary op l r) =
    let
        tl = checkExpression ge le l
        tr = checkExpression ge le r
        t1 = texprType tl
        t2 = texprType tr
    in
        case op of
            Plus -> let mtype = numType t1 t2 
                in TExpression {texprType = mtype, texprNode = TBinary op (coerceAssign mtype tl) (coerceAssign mtype tr)}
            Minus -> let mtype = numType t1 t2 
                in TExpression {texprType = mtype, texprNode = TBinary op (coerceAssign mtype tl) (coerceAssign mtype tr)}
            Mul -> let mtype = numType t1 t2
                in TExpression {texprType = mtype, texprNode = TBinary op (coerceAssign mtype tl) (coerceAssign mtype tr)}
            Div -> let mtype = numType t1 t2
                in TExpression {texprType = mtype, texprNode = TBinary op (coerceAssign mtype tl) (coerceAssign mtype tr)}
            Gt -> if isNumeric t1 && isNumeric t2
                then let mtype = numType t1 t2
                in TExpression {texprType = PrimitiveType Int, texprNode = TBinary op (coerceAssign mtype tl) (coerceAssign mtype tr)}
                else error "Relational op expects numeric operands"
            Lt -> if isNumeric t1 && isNumeric t2
                then let mtype = numType t1 t2
                in TExpression {texprType = PrimitiveType Int, texprNode = TBinary op (coerceAssign mtype tl) (coerceAssign mtype tr)}
                else error "Relational op expects numeric operands"
            Gte -> if isNumeric t1 && isNumeric t2
                then let mtype = numType t1 t2
                in TExpression {texprType = PrimitiveType Int, texprNode = TBinary op (coerceAssign mtype tl) (coerceAssign mtype tr)}
                else error "Relational op expects numeric operands"
            Lte -> if isNumeric t1 && isNumeric t2
                then let mtype = numType t1 t2
                in TExpression {texprType = PrimitiveType Int, texprNode = TBinary op (coerceAssign mtype tl) (coerceAssign mtype tr)}
                else error "Relational op expects numeric operands"
            Eq -> if isNumeric t1 && isNumeric t2
                then let mtype = numType t1 t2
                in TExpression {texprType = PrimitiveType Int, texprNode = TBinary op (coerceAssign mtype tl) (coerceAssign mtype tr)}
                else if isPointer t1 && isPointer t2
                    then
                        TExpression {texprType = PrimitiveType Int, texprNode = TBinary op tl tr}
                    else
                        error "Eq expects both numeric or both pointers"
            Neq -> if isNumeric t1 && isNumeric t2
                then let mtype = numType t1 t2
                in TExpression {texprType = PrimitiveType Int, texprNode = TBinary op (coerceAssign mtype tl) (coerceAssign mtype tr)}
                else if isPointer t1 && isPointer t2
                    then
                        TExpression {texprType = PrimitiveType Int, texprNode = TBinary op tl tr}
                    else
                        error "Eq expects both numeric or both pointers"

-- Expression addres of checking
checkExpression ge le (AddressOf expr) =
    let te = checkExpression ge le expr
    in
        if isLValue expr
            then TExpression {texprType = PointerType (texprType te), texprNode = TAddressOf te}
            else error "Address-of expects an lvalue"

-- Expression deref checking
checkExpression ge le (Deref expr) =
    let te = checkExpression ge le expr
    in
        case texprType te of
            PointerType t -> TExpression {texprType = t, texprNode = TDeref te}
            _ -> error "Cannot dereference non-pointer"

-- Expression cast checking
checkExpression ge le (Cast typ expr) =
    let
        texpr = checkExpression ge le expr
    in
        TExpression {texprType=typ, texprNode=TCast typ texpr}

-- Expression call checking
checkExpression ge le (Call name args) =
    let
        namestr = case name of
            Var v -> v
            _ -> error "Call expects function name"
        (functype, argstypes) = case Map.lookup namestr ge of
            Just (f, a) -> (f, a)
            Nothing -> error ("Function with name " ++ namestr ++ " not exist")
        check :: [Expression] -> [Type] -> [TExpression]
        check [] [] = []
        check e [] = error ("Invalid args in function " ++ namestr)
        check [] t = error ("Invalid args in function " ++ namestr)
        check (e:es) (t:ts) =
            let
                texpr = checkExpression ge le e
                typ = texprType texpr
            in
                case (typ, t) of
                    (PrimitiveType Int, PrimitiveType Long) ->
                        let
                            casted = 
                                TExpression
                                    {texprType = PrimitiveType Long
                                    , texprNode = TCast (PrimitiveType Long) texpr
                                    }
                        in
                            casted : check es ts
                    _ -> if texprType texpr /= t
                        then error ("Invalid arg type in function " ++ namestr)
                        else texpr : check es ts
    in
        TExpression {texprType = functype, texprNode = TCall namestr (check args argstypes)}

-- =============================
-- Helpers
-- =============================
isLValue :: Expression -> Bool
isLValue (Var _) = True
isLValue (Deref _) = True
isLValue (Cast _ e) = isLValue e
isLValue _ = False

isNumeric :: Type -> Bool
isNumeric (PrimitiveType Int) = True
isNumeric (PrimitiveType Long) = True
isNumeric _ = False

isPointer :: Type -> Bool
isPointer (PointerType _) = True
isPointer _ = False

numType :: Type -> Type -> Type
numType t1 t2 =
    case (t1, t2) of
        (PrimitiveType Long, PrimitiveType Long) -> PrimitiveType Long
        (PrimitiveType Int , PrimitiveType Long) -> PrimitiveType Long
        (PrimitiveType Long, PrimitiveType Int ) -> PrimitiveType Long
        (PrimitiveType Int , PrimitiveType Int ) -> PrimitiveType Int
        _ -> error "Arithmetic op expects numeric operands"

coerceAssign :: Type -> TExpression -> TExpression
coerceAssign lhs te =
    case (lhs, texprType te) of
        (t1, t2) | t1 == t2 -> te
        (PrimitiveType Long, PrimitiveType Int) -> TExpression { texprType = lhs, texprNode = TCast lhs te }
        (PrimitiveType Int, PrimitiveType Long) -> TExpression { texprType = lhs, texprNode = TCast lhs te }
        _ -> error "type mismatch in initializer"

