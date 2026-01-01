module IR.Lower where

import AST.Type
import AST.Lit
import AST.Operator
import AST.UnaryOp
import IR.Builder
import IR.Types
import Semantic.Types

lowerExpression :: Builder -> TExpression -> (Builder, Val)

lowerExpression b texpr =
    case texprNode texpr of
        TVar name -> lowerVar b (texprType texpr) name
        TLiteral (LNum val) -> lowerLiteral b (texprType texpr) val
        TBinary op lhs rhs -> lowerBinary b (texprType texpr) op lhs rhs
        TAddressOf e -> lowerAddressOf b (texprType texpr) e
        TDeref e -> lowerDeref b (texprType texpr) e
        TCast to e -> lowerCast b to e
        TUnaryOp u e -> lowerUnary b (texprType texpr) u e
        TCall n es -> lowerCall b (texprType texpr) n es
        _ -> error ("lowerExpression: unhandled node " ++ show (texprNode texpr))

lowerCall :: Builder -> Type -> String -> [TExpression] -> (Builder, Val)
lowerCall b t name exprs =
    let
        f :: Builder -> [TExpression] -> (Builder, [Val])
        f b [] = (b, [])
        f b (e:es) =
            let
                (newb1, v) = lowerExpression b e
                (newb2, vals) = f newb1 es
            in
                (newb2, v : vals)
        (newb1, vals) = f b exprs
    in
        case t of
            PrimitiveType Void -> (emit (ICall Nothing name vals) newb1, VConst 0)
            _ ->
                let
                    (newb2, curtemp) = freshTemp newb1
                in
                    (emit (ICall (Just curtemp) name vals) newb2, VTemp curtemp)

lowerUnary :: Builder -> Type -> UnaryOp -> TExpression -> (Builder, Val)
lowerUnary b t u texpr =
    let
        (newb1, v) = lowerExpression b texpr
        (newb2, curtemp) = freshTemp newb1
    in
        (emit (IUnaryOp curtemp t u v) newb2, VTemp curtemp)

lowerCast :: Builder -> Type -> TExpression -> (Builder, Val)
lowerCast b toType texpr =
    let
        (newb1, v) = lowerExpression b texpr
    in
        if texprType texpr == toType
            then
                (newb1, v)
            else
                let
                    (newb2, curtemp) = freshTemp newb1
                in
                    (emit (ICast curtemp toType v) newb2, VTemp curtemp)

lowerDeref :: Builder -> Type -> TExpression -> (Builder, Val)
lowerDeref b t texpr =
    let
        (newb1, curval) = lowerExpression b texpr
        (newb2, curtemp) = ensureTemp newb1 curval
        (newb3, curtemp1) = freshTemp newb2
        curinstr = ILoad curtemp1 t (ATemp curtemp)
    in
        (emit curinstr newb3, VTemp curtemp1)

lowerAddressOf :: Builder -> Type -> TExpression -> (Builder, Val)
lowerAddressOf b t texpr =
    case texprNode texpr of
        TVar name ->
            let
                (newb1, curtemp) = freshTemp b
            in
                (emit (IAddrOf curtemp name) newb1, VTemp curtemp)
        TDeref p ->
            let
                (newb1, pv) = lowerExpression b p
                (newb2, tp) = ensureTemp newb1 pv
            in
                (newb2, VTemp tp) 
        TCast _ e -> lowerAddressOf b t e
        _ -> error "Address-of expects an lvalue"

lowerBinary :: Builder -> Type -> Operator -> TExpression -> TExpression -> (Builder, Val)
lowerBinary b typ op lhs rhs =
    let
        (newb1, lexprVal) = lowerExpression b lhs
        (newb2, rexprVal) = lowerExpression newb1 rhs
        (newb3, curtemp) = freshTemp newb2
        bininstr = IBin curtemp typ op lexprVal rexprVal
    in
        (emit bininstr newb3, VTemp curtemp)

lowerVar :: Builder -> Type -> String -> (Builder, Val)
lowerVar b typ name =
    let
        (newb, curtemp) = freshTemp b
        varinstr = ILoad curtemp typ (AVar name)
    in
        (emit varinstr newb, VTemp curtemp)

lowerLiteral :: Builder -> Type -> String -> (Builder, Val)
lowerLiteral b typ val =
    (b, VConst (read val :: Integer))


ensureTemp :: Builder -> Val -> (Builder, Temp)
ensureTemp b v =
    case v of
        VTemp t -> (b, t)
        VConst _ ->
            let
                (newb1, t) = freshTemp b
            in
                (emit (IMov t v) newb1, t)
