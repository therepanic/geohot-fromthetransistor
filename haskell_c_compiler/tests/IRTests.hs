module IRTests where

import AST.Type
import AST.Expression
import AST.Lit
import AST.Operator
import AST.Statement
import AST.UnaryOp
import IR.Builder
import IR.Lower
import IR.Types
import Semantic.Types
import Tests

b0 :: Builder
b0 = Builder { nextTemp = 0, nextLabel = 0, code = [] }

mkVar :: String -> Type -> TExpression
mkVar n t = TExpression { texprType = t, texprNode = TVar n }

mkNum :: String -> TExpression
mkNum s = TExpression { texprType = PrimitiveType Int, texprNode = TLiteral (LNum s) }

mkBin :: Operator -> TExpression -> TExpression -> Type -> TExpression
mkBin op l r t = TExpression { texprType = t, texprNode = TBinary op l r }

mkUn :: UnaryOp -> TExpression -> Type -> TExpression
mkUn u e t = TExpression { texprType = t, texprNode = TUnaryOp u e }

ir :: [TStatement] -> [Instr]
ir stmts = reverse (code (lowerBlock b0 stmts))

-- int x = 5;  -> IStore (AVar "x") int 5
test_vardecl_store_const :: IO ()
test_vardecl_store_const = do
    let t = PrimitiveType Int
        stmts = [TVarDecl "x" t (Just (TExpression t (TLiteral (LNum "5"))))]
        xs = ir stmts
    assert "vardecl lowers to store const" $
        case xs of
            [IStore (AVar "x") ty (VConst 5)] -> ty == t
            _ -> False

-- if (a < b) { return 1; } else { return 0; }
-- must produce a relational ICondJump Lt ... and two returns, and end label.
test_if_relop_condjump :: IO ()
test_if_relop_condjump = do
    let
        tI = PrimitiveType Int
        a = mkVar "a" tI
        b = mkVar "b" tI
        cond = TExpression tI (TBinary Lt a b)
        stmts =
            [TIf cond
                [TReturn (Just (TExpression tI (TLiteral (LNum "1"))))]
                [TReturn (Just (TExpression tI (TLiteral (LNum "0"))))]
            ]
        xs = ir stmts
    assert "if lowers to relational condjump + both returns" $
        any isRelCJ xs && any isRet1 xs && any isRet0 xs && any isEndLabel xs
    where
        isRelCJ (ICondJump Lt _ _ _ _ _) = True
        isRelCJ _ = False

        isRet1 (IReturn (Just (_, VConst 1))) = True
        isRet1 _ = False

        isRet0 (IReturn (Just (_, VConst 0))) = True
        isRet0 _ = False

        isEndLabel (ILabel _) = True
        isEndLabel _ = False

-- while (!(a == 0)) { a = a - 1; }
test_while_not_flip :: IO ()
test_while_not_flip = do
    let tI = PrimitiveType Int
        a = mkVar "a" tI
        zero = TExpression tI (TLiteral (LNum "0"))
        eq0 = TExpression tI (TBinary Eq a zero)
        cond = mkUn Not eq0 tI
        dec = TExpression tI (TBinary Minus a (TExpression tI (TLiteral (LNum "1"))))
        stmts =
            [TWhile cond
                [TAssign a dec]
            ]
        xs = ir stmts
    assert "while lowers with condjump + loop back jump" $
        any isCondJump xs && any isJump xs && any isStore xs
    where
        isCondJump (ICondJump {}) = True
        isCondJump _ = False

        isJump (IJump _) = True
        isJump _ = False

        isStore (IStore {}) = True
        isStore _ = False

main :: IO ()
main = do
    putStrLn "Running IR tests..."
    test_vardecl_store_const
    test_if_relop_condjump
    test_while_not_flip
    putStrLn "OK"
