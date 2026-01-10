module IR.Lower where

import Data.List (foldl')
import AST.Type
import AST.Lit
import AST.Operator
import AST.UnaryOp
import IR.Builder
import IR.Types
import Semantic.Types

-- =============================
-- Statements lowering
-- =============================
lowerStatement :: Builder -> TStatement -> Builder
lowerStatement b tstmt =
    case tstmt of
        TExprStmt e -> lowerExprStmt b e
        TReturn e -> lowerReturn b e
        TVarDecl n t e -> lowerVarDecl b n t e
        TAssign l r -> lowerAssign b l r
        TWhile c body -> lowerWhile b c body
        TIf c t e -> lowerIf b c t e

-- Statement assign lowering
lowerAssign :: Builder -> TExpression -> TExpression -> Builder
lowerAssign b lhs rhs =
    let
        (newb1, lhsaddr) = lowerAddr b lhs
        (newb2, rhsv) = lowerExpression newb1 rhs
    in
        emit (IStore lhsaddr (texprType lhs) rhsv) newb2

-- Statement while lowering
lowerWhile :: Builder -> TExpression -> [TStatement] -> Builder
lowerWhile b cond body =
    let
        (newb1, beginLabel) = freshLabel b
        (newb2, midLabel) = freshLabel newb1
        (newb3, stopLabel) = freshLabel newb2
        newb4 = emit (ILabel beginLabel) newb3
        newb5 = lowerCondJump newb4 cond midLabel stopLabel
        newb6 = emit (ILabel midLabel) newb5
        newb7 = lowerBlock newb6 body
        newb8 = emit (IJump beginLabel) newb7
    in
        emit (ILabel stopLabel) newb8

-- Statement if lowering
lowerIf :: Builder -> TExpression -> [TStatement] -> [TStatement] -> Builder
lowerIf b cond thn elze =
    let
        (newb1, thenLabel) = freshLabel b
        (newb2, elseLabel) = freshLabel newb1
        (newb3, endLabel) = freshLabel newb2
        newb4 = lowerCondJump newb3 cond thenLabel elseLabel
        newb5 = emit (ILabel thenLabel) newb4
        newb6 = lowerBlock newb5 thn
        newb7 = emit (IJump endLabel) newb6
        newb8 = emit (ILabel elseLabel) newb7
        newb9 = lowerBlock newb8 elze
    in
        emit (ILabel endLabel) newb9

-- Statement var declaration lowering
lowerVarDecl :: Builder -> String -> Type -> Maybe TExpression -> Builder
lowerVarDecl b name t Nothing = b
lowerVarDecl b name t (Just texpr) =
    let
        (newb1, v) = lowerExpression b texpr
    in
        emit (IStore (AVar name) t v) newb1

-- Statement return lowering
lowerReturn :: Builder -> Maybe TExpression -> Builder
lowerReturn b Nothing = emit (IReturn Nothing) b
lowerReturn b (Just texpr) =
    let
        t = texprType texpr
        (newb1, v) = lowerExpression b texpr
    in
        emit (IReturn (Just (t, v))) newb1

-- Statement exprstmt lowering
lowerExprStmt :: Builder -> TExpression -> Builder
lowerExprStmt b e = fst (lowerExpression b e)

-- Block lowering
lowerBlock :: Builder -> [TStatement] -> Builder
lowerBlock = foldl' lowerStatement

-- =============================
-- Expressions lowering
-- =============================
lowerExpression :: Builder -> TExpression -> (Builder, Val)
lowerExpression b texpr =
    case texprNode texpr of
        TVar name -> lowerVar b (texprType texpr) name
        TLiteral (LNum val) -> lowerLiteral b (texprType texpr) val
        TBinary op lhs rhs -> lowerBinary b (texprType texpr) op lhs rhs
        TAddressOf e -> lowerAddressOf b e
        TDeref e -> lowerDeref b (texprType texpr) e
        TCast to e -> lowerCast b to e
        TUnaryOp u e -> lowerUnary b (texprType texpr) u e
        TCall n es -> lowerCall b (texprType texpr) n es
        _ -> error ("lowerExpression: unhandled node " ++ show (texprNode texpr))

-- Expression call lowering
lowerCall :: Builder -> Type -> String -> [TExpression] -> (Builder, Val)
lowerCall b t name exprs =
    let
        f :: Builder -> [TExpression] -> (Builder, [(Type, Val)])
        f b [] = (b, [])
        f b (e:es) =
            let
                (newb1, v) = lowerExpression b e
                (newb2, vals) = f newb1 es
            in
                (newb2, (texprType e, v) : vals)
        (newb1, vals) = f b exprs
    in
        case t of
            PrimitiveType Void -> (emit (ICall Nothing t name vals) newb1, VConst 0)
            _ ->
                let
                    (newb2, curtemp) = freshTemp newb1
                in
                    (emit (ICall (Just curtemp) t name vals) newb2, VTemp curtemp)

-- Expression unary lowering
lowerUnary :: Builder -> Type -> UnaryOp -> TExpression -> (Builder, Val)
lowerUnary b t u texpr =
    let
        (newb1, v) = lowerExpression b texpr
        (newb2, curtemp) = freshTemp newb1
    in
        (emit (IUnaryOp curtemp t u v) newb2, VTemp curtemp)

-- Expression cast lowering
lowerCast :: Builder -> Type -> TExpression -> (Builder, Val)
lowerCast b toType texpr =
    let
        fromType = texprType texpr
        (newb1, v) = lowerExpression b texpr
    in
        if fromType == toType
            then
                (newb1, v)
            else
                let
                    (newb2, curtemp) = freshTemp newb1
                in
                    (emit (ICast curtemp fromType toType v) newb2, VTemp curtemp)

-- Expression deref lowering
lowerDeref :: Builder -> Type -> TExpression -> (Builder, Val)
lowerDeref b t texpr =
    let
        (newb1, curval) = lowerExpression b texpr
        (newb2, curtemp) = ensureTemp newb1 (texprType texpr) curval
        (newb3, curtemp1) = freshTemp newb2
        curinstr = ILoad curtemp1 t (ATemp curtemp)
    in
        (emit curinstr newb3, VTemp curtemp1)

-- Expression address of lowering
lowerAddressOf :: Builder -> TExpression -> (Builder, Val)
lowerAddressOf b texpr =
    let
        (newb1, lhsaddr) = lowerAddr b texpr
    in
        case lhsaddr of
            ATemp tp -> (newb1, VTemp tp)
            AVar name ->
                let
                    (newb2, curtemp) = freshTemp newb1
                in
                    (emit (IAddrOf curtemp (texprType texpr) name) newb2, VTemp curtemp)

-- Expression binary lowering
lowerBinary :: Builder -> Type -> Operator -> TExpression -> TExpression -> (Builder, Val)
lowerBinary b typ op lhs rhs =
    let
        (newb1, lexprVal) = lowerExpression b lhs
        (newb2, rexprVal) = lowerExpression newb1 rhs
        (newb3, curtemp) = freshTemp newb2
        widthTy = if isRelOp op then texprType lhs else typ
        bininstr = IBin curtemp widthTy op lexprVal rexprVal
    in
        (emit bininstr newb3, VTemp curtemp)

-- Expression var lowering
lowerVar :: Builder -> Type -> String -> (Builder, Val)
lowerVar b typ name =
    let
        (newb, curtemp) = freshTemp b
        varinstr = ILoad curtemp typ (AVar name)
    in
        (emit varinstr newb, VTemp curtemp)

-- Expression literal lowering
lowerLiteral :: Builder -> Type -> String -> (Builder, Val)
lowerLiteral b typ val =
    (b, VConst (read val :: Integer))

-- =============================
-- Helpers
-- =============================
ensureTemp :: Builder -> Type -> Val -> (Builder, Temp)
ensureTemp b ty v =
    case v of
        VTemp t -> (b, t)
        VConst _ ->
            let
                (newb1, t) = freshTemp b
            in
                (emit (IMov t ty v) newb1, t)

lowerAddr :: Builder -> TExpression -> (Builder, Addr)
lowerAddr b te =
    case texprNode te of
        TVar name ->
            (b, AVar name)
        TDeref p ->
            let
                (newb1, pv) = lowerExpression b p
                (newb2, tp) = ensureTemp newb1 (texprType p) pv
            in
                (newb2, ATemp tp)
        TCast _ e ->
            lowerAddr b e
        _ ->
            error "Assignment expects lvalue on the left"

lowerCondJump :: Builder -> TExpression -> Label -> Label -> Builder
lowerCondJump b cond lTrue lFalse =
    case texprNode cond of
        TUnaryOp Not e -> lowerCondJump b e lFalse lTrue
        TBinary op lhs rhs | isRelOp op ->
            let
                (newb1, lhsv) = lowerExpression b lhs
                (newb2, rhsv) = lowerExpression newb1 rhs
            in
                emit (ICondJump op (texprType lhs) lhsv rhsv lTrue lFalse) newb2
        _ ->
            let
                (newb1, v) = lowerExpression b cond
            in
                emit (ICondJump Neq (texprType cond) v (VConst 0) lTrue lFalse) newb1

isRelOp :: Operator -> Bool
isRelOp op =
    case op of
        Eq  -> True
        Neq -> True
        Lt  -> True
        Lte  -> True
        Gt  -> True
        Gte  -> True
        _   -> False