module ParserTests where

import qualified AST.UnaryOp as U

import AST.Expression
import AST.Operator
import AST.Type
import AST.Lit
import AST.Statement
import Lexer.Token
import Parser.Parser
import AST.VarDecl
import Tests

p0 = Pos 0 0
i s = TokIdent s p0
n s = TokNum s p0
t f = f p0

lp = t TokLParen
rp = t TokRParen
lb = t TokLBrace
rb = t TokRBrace
sc = t TokSemicolon
cm = t TokComma
pl = t TokPlus
mn = t TokMinus
st = t TokStar
as = t TokAssign
nt = t TokNot
eq = t TokEqEq
le = t TokLte
eof = TokEOF p0

-- a + b*c == d
test_precedence :: IO ()
test_precedence =
    assert "precedence" $
        case parseProgram [i "a", pl, i "b", st, i "c", eq, i "d", sc, eof] of
                [ExprStmt (Binary Eq (Binary Plus (Var "a") (Binary Mul (Var "b") (Var "c")))
                    (Var "d"))] -> True
                _ -> False

-- *&-x; !-1;
test_unary :: IO ()
test_unary =
    assert "unary" $
        case parseProgram
        [st, t TokAmpersand, mn, i "x", sc, nt, mn, n "1", sc, eof] of
            [ExprStmt (Deref (AddressOf (UnaryOp U.Neg (Var "x")))), ExprStmt (UnaryOp U.Not (UnaryOp U.Neg (Literal (LNum "1"))))] -> True
            _ -> False

-- int x = 1+2; if (x <= 3) { return x; } else { return 0; }
test_statements :: IO ()
test_statements =
    assert "statements" $
        case parseProgram [i "int", i "x", as, n "1", pl, n "2", sc, i "if", lp, i "x", le, n "3", rp
            , lb, i "return", i "x", sc, rb, i "else", lb, i "return", n "0", sc, rb, eof ] of
                [VarDeclStmt (VarDecl "x" (PrimitiveType Int) _), If _ [Return (Just (Var "x"))] [Return (Just (Literal (LNum "0")))]] -> True
                _ -> False

main :: IO ()
main = do
    test_precedence
    test_unary
    test_statements
    putStrLn "OK"
