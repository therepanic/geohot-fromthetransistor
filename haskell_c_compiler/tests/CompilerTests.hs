module CompilerTests where

import Tests
import Compiler (compile)
import Data.List (isInfixOf)
import Control.Exception (evaluate)

-- helpers
assertHas :: String -> String -> IO ()
assertHas needle hay =
    assert ("expected output to contain: " <> show needle <> "\n--- output ---\n" <> take 800 hay <> "\n---")
        (needle `isInfixOf` hay)

assertNotHas :: String -> String -> IO ()
assertNotHas needle hay =
    assert ("expected output NOT to contain: " <> show needle)
        (not (needle `isInfixOf` hay))

test_compile_minimal_return :: IO ()
test_compile_minimal_return = do
    let src = "int main(){ return 0; }"
        out = compile src
    assertHas "main" out
    assertHas "bx lr" out
    assertHas "ldr" out

test_compile_function_call :: IO ()
test_compile_function_call = do
    let
        src = unlines [ "int foo(int a){ return a; }", "int main(){ return foo(1); }"]
        out = compile src
    assertHas "foo" out
    assertHas "bl foo" out
    assertHas "bx lr" out

test_compile_if_relop_branches :: IO ()
test_compile_if_relop_branches = do
    let src = "int main(){ int a = 1; int b = 2; if (a < b) { return 1; } else { return 0; } }"
        out = compile src
    assertHas "cmp" out
    assertHas "\nb" out
    assertHas "bx lr" out

test_compile_semantic_error :: IO ()
test_compile_semantic_error = do
    let
        src = "int main(){ return; }"
    assertThrows "semantic error should throw" $ evaluate (length (compile src)) >> pure ()

test_compile_while_loop :: IO ()
test_compile_while_loop = do
    let src = "int main(){ int a = 3; while (a > 0) { a = a - 1; } return a; }"
        out = compile src
    assertHas "cmp" out
    assertHas "\nb" out
    assertHas "sub" out
    assertHas "bx lr" out

main :: IO ()
main = do
    putStrLn "Running compiler tests..."
    test_compile_minimal_return
    test_compile_function_call
    test_compile_if_relop_branches
    test_compile_semantic_error
    test_compile_while_loop
    putStrLn "OK"
