module Compiler where

import ARM7.Codegen
import ARM7.Types
import ARM7.Pretty (prettyProgram)
import Control.Monad.State.Strict (State, evalState, get, put)
import Data.List (foldl')
import IR.Builder
import IR.Lower (lowerStatement)
import IR.Types
import Lexer.Lexer as L
import Lexer.Token
import Parser.Parser (parseProgram)
import Semantic.Check (checkProgram)
import Semantic.Types

compile :: String -> String
compile src =
    let
        tokens = L.lex (Pos {line = 0, col = 0}) src
        ast = parseProgram tokens
        tprog = checkProgram ast
        funAsms = genProgram tprog
    in
        prettyProgram funAsms

genProgram :: TProgram -> [AsmInstr]
genProgram (TProgram funs) =
    let
        lowered = evalState (mapM lowerOne funs) 0
        (asms, _) = foldl' genStep ([], 0) lowered
    in
        asms
    where
        lowerOne :: TFunction -> State Int (TFunction, [Instr])
        lowerOne fn@TFunction{tfBody = body} = do
            lbl0 <- get
            let 
                b0 = Builder {nextTemp = 0, nextLabel = lbl0, code = []}
                b1 = foldl' lowerStatement b0 body
                ir = reverse (code b1)
            put (nextLabel b1)
            pure (fn, ir)

        genStep :: ([AsmInstr], Int) -> (TFunction, [Instr]) -> ([AsmInstr], Int)
        genStep (acc, asmSeed) (TFunction{tfName = name, tfParams = params}, ir) =
            let
                (asm, asmSeed') = genFunction params ir asmSeed
            in
                (acc ++ (ARM7.Types.Label (LabelName name) : asm), asmSeed')
