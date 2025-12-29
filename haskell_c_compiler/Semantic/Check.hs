module Check where

import qualified Data.Map.Strict as Map

import AST.Expression
import AST.Operator
import AST.Statement
import AST.VarDecl
import AST.Type
import AST.Lit
import Semantic.Typed
import Semantic.Scope

-- checkProgram :: [Statement] -> TProgram
-- checkFunction :: GlobalEnv -> Statement -> TFunction
-- checkStatement :: GlobalEnv -> Type -> LocalEnv -> Statement -> (LocalEnv, TStatement)
-- checkStatement ge _ (l:le) (VarDeclStmt (VarDecl name typ init)) =
--     if Map.member name l then error ("Variable with name " ++ name + " already exists")
--     else let
--         m = Map.insert name typ l
--     in
--         case init of

--         (m:le, TVarDecl name typ init)

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

-- =============================
-- Expressions checking
-- =============================

checkExpr :: GlobalEnv -> LocalEnv -> Expression -> TExpression
checkExpr _ le (Var name) =
    let
        containsVar :: LocalEnv -> String -> TExpression
        containsVar [] name = error ("There is no variable with name " ++ name)
        containsVar (l:rest) name =
            case Map.lookup name l of
                Just v -> TExpression {texprType=v, texprNode=TVar name}
                Nothing -> containsVar rest name
    in
        containsVar le name
checkExpr _ le (Literal lit) = TExpression {texprType=PrimitiveType Int, texprNode=TLiteral lit}
checkExpr ge le (UnaryOp s expr) =
    let
        texpr = checkExpr ge le expr
    in
        TExpression {texprType=texprType texpr, texprNode=TUnaryOp s texpr}
checkExpr ge le (Binary op l r) =
    let
        tl = checkExpr ge le l
        tr = checkExpr ge le r
        t1 = texprType tl
        t2 = texprType tr
    in
        case op of
            Plus -> let mtype = numType t1 t2 in TExpression {texprType = mtype, texprNode = TBinary op tl tr}
            Minus -> let mtype = numType t1 t2 in TExpression {texprType = mtype, texprNode = TBinary op tl tr}
            Mul -> let mtype = numType t1 t2 in TExpression {texprType = mtype, texprNode = TBinary op tl tr}
            Div -> let mtype = numType t1 t2 in TExpression {texprType = mtype, texprNode = TBinary op tl tr}
            Gt -> if isNumeric t1 && isNumeric t2
                then TExpression {texprType = PrimitiveType Int, texprNode = TBinary op tl tr}
                else error "Relational op expects numeric operands"
            Lt -> if isNumeric t1 && isNumeric t2
                then TExpression {texprType = PrimitiveType Int, texprNode = TBinary op tl tr}
                else error "Relational op expects numeric operands"
            Gte -> if isNumeric t1 && isNumeric t2
                then TExpression {texprType = PrimitiveType Int, texprNode = TBinary op tl tr}
                else error "Relational op expects numeric operands"
            Lte -> if isNumeric t1 && isNumeric t2
                then TExpression {texprType = PrimitiveType Int, texprNode = TBinary op tl tr}
                else error "Relational op expects numeric operands"
            Eq -> if (isNumeric t1 && isNumeric t2) || (isPointer t1 && isPointer t2)
                then TExpression {texprType = PrimitiveType Int, texprNode = TBinary op tl tr}
                else error "Eq expects both numeric or both pointers"
            Neq -> if (isNumeric t1 && isNumeric t2) || (isPointer t1 && isPointer t2)
                then TExpression {texprType = PrimitiveType Int, texprNode = TBinary op tl tr}
                else error "Eq expects both numeric or both pointers"
checkExpr ge le (AddressOf expr) =
    let te = checkExpr ge le expr
    in
        if isLValue expr
            then TExpression {texprType = PointerType (texprType te), texprNode = TAddressOf te}
            else error "Address-of expects an lvalue"
checkExpr ge le (Deref expr) =
    let te = checkExpr ge le expr
    in
        case texprType te of
            PointerType t -> TExpression {texprType = t, texprNode = TDeref te}
            _ -> error "Cannot dereference non-pointer"
checkExpr ge le (Cast typ expr) =
    let
        texpr = checkExpr ge le expr
    in
        TExpression {texprType=typ, texprNode=TCast typ texpr}

checkExpr ge le (Call name args) =
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
                texpr = checkExpr ge le e
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
