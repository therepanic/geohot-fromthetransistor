module SemanticTests where

import AST.Expression
import AST.Lit
import AST.Type
import AST.Statement
import AST.VarDecl
import Control.Exception (evaluate)
import Semantic.Types
import Semantic.Check
import Semantic.Scope
import Semantic.Collect
import Tests

intT, longT, voidT :: Type
intT  = PrimitiveType Int
longT = PrimitiveType Long
voidT = PrimitiveType Void

forceSem :: [Statement] -> IO ()
forceSem prog =
    evaluate (length (show (checkProgram prog))) >> pure ()

test_dup_var_decl :: IO ()
test_dup_var_decl =
    assertThrows "duplicate var decl" $
    forceSem
        [Function intT "main" []
        [VarDeclStmt (VarDecl "x" intT Nothing), VarDeclStmt (VarDecl "x" intT Nothing)]]

test_return_without_value_nonvoid :: IO ()
test_return_without_value_nonvoid =
    assertThrows "return without value in non-void" $
    forceSem
        [Function intT "main" [] [Return Nothing]]

main :: IO ()
main = do
    putStrLn "Running semantic tests..."

    test_dup_var_decl
    test_return_without_value_nonvoid

    putStrLn "OK"
