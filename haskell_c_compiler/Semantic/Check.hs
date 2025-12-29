module Check where

import qualified Data.Map.Strict as Map

import AST.Expression
import AST.Type
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
isLValue :: Expression -> Bool
isLValue (Var _) = True
isLValue (Deref _) = True
isLValue (Cast _ e) = isLValue e
isLValue _ = False

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
        mtype = case (texprType tl, texprType tr) of
            (PrimitiveType Long, PrimitiveType Long) -> PrimitiveType Long
            (PrimitiveType Int, PrimitiveType Long) -> PrimitiveType Long
            (PrimitiveType Long, PrimitiveType Int) -> PrimitiveType Long
            (PrimitiveType Int , PrimitiveType Int) -> PrimitiveType Int
            _ -> error "Binary op expects numeric operands"
    in
        TExpression {texprType=mtype, texprNode = TBinary op tl tr}
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
