module LexerTests where

import Lexer.Lexer as L
import Lexer.Token
import Tests
import Control.Exception (evaluate)

p :: Int -> Int -> Pos
p l c = Pos { line = l, col = c }

test_relational_operators :: IO ()
test_relational_operators = do
    let toks = L.lex (p 1 1) "<= >= == !="
    assertEq "relational operators"
        [TokLte (p 1 1), TokGte (p 1 4), TokEqEq (p 1 7), TokNeq (p 1 10), TokEOF (p 1 12)]
        toks

test_ident_and_number :: IO ()
test_ident_and_number = do
    let toks = L.lex (p 1 1) "foo=12+bar;"
    assertEq "ident / number / operators"
        [TokIdent "foo" (p 1 1)
        , TokAssign (p 1 4)
        , TokNum "12" (p 1 5)
        , TokPlus (p 1 7)
        , TokIdent "bar" (p 1 8)
        , TokSemicolon (p 1 11)
        , TokEOF (p 1 12)]
        toks

test_positions_with_newline :: IO ()
test_positions_with_newline = do
    let toks = L.lex (p 1 1) "a\n  b"
    assertEq "newline position tracking"
        [TokIdent "a" (p 1 1)
        , TokIdent "b" (p 2 3)
        , TokEOF (p 2 4)]
        toks

test_unexpected_character :: IO ()
test_unexpected_character = do
    assertThrows "unexpected character"
        (evaluate (L.lex (p 1 1) "@"))

main :: IO ()
main = do
    putStrLn "Running lexer tests..."

    test_relational_operators
    test_ident_and_number
    test_positions_with_newline
    test_unexpected_character

    putStrLn "OK"
