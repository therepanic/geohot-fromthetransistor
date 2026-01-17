module CodegenTests where

import ARM7.Codegen
import ARM7.Frame
import ARM7.Types
import AST.Operator
import AST.Type
import IR.Types
import Tests

intT, longT :: Type
intT  = PrimitiveType Int
longT = PrimitiveType Long

containsInOrder :: (AsmInstr -> Bool) -> [AsmInstr] -> Bool
containsInOrder _ [] = False
containsInOrder p xs = any p xs

subseq :: [(AsmInstr -> Bool)] -> [AsmInstr] -> Bool
subseq = go
    where
    go [] _ = True
    go _ [] = False
    go (p:ps') (x:xs')
        | p x  = go ps' xs'
        | otherwise = go (p:ps') xs'

isLabel :: Label -> AsmInstr -> Bool
isLabel l (ARM7.Types.Label l') = l == l'
isLabel _ _ = False

isBAl :: Label -> AsmInstr -> Bool
isBAl l (B Al l') = l == l'
isBAl _ _ = False

isBl :: String -> AsmInstr -> Bool
isBl s (Bl s') = s == s'
isBl _ _ = False

isPushPrologue :: AsmInstr -> Bool
isPushPrologue (Push rs) = rs == [R4, R5, FP, LR]
isPushPrologue _ = False

isSetFP :: AsmInstr -> Bool
isSetFP (Mov Al FP (OpReg SP)) = True
isSetFP _ = False

endsWithPopBx :: [AsmInstr] -> Bool
endsWithPopBx xs =
    case reverse xs of
        (Bx LR : Pop rs : _) -> rs == [R4, R5, FP, LR]
        _ -> False

-- IR: t0=7; return t0;
test_return_label_and_function_shell :: IO ()
test_return_label_and_function_shell = do
    let t0 = Temp 0
        ir = [IMov t0 intT (VConst 7), IReturn (Just (intT, VTemp t0))]
        asm = fst (genFunction [] ir 0)
        rtrn = IR.Types.Label 1

    assert "prologue exists (Push + FP=SP)" $
        containsInOrder isPushPrologue asm && containsInOrder isSetFP asm

    assert "return branches to synthetic return label" $
        containsInOrder (isBAl rtrn) asm

    assert "return label is emitted" $
        containsInOrder (isLabel rtrn) asm

    assert "epilogue ends with Pop + Bx LR" $
        endsWithPopBx asm

-- Int->Long cast must sign-extend: Asr R1,R0,#31
test_cast_int_to_long_signext :: IO ()
test_cast_int_to_long_signext = do
    let t0 = Temp 0
        ir = [ICast t0 intT longT (VConst 1), IReturn Nothing]
        asm = fst (genFunction [] ir 0)
    assert "ICast int->long emits Asr R1 R0 #31" $
        containsInOrder (\i -> case i of Asr R1 R0 (OpImm 31) -> True; _ -> False) asm

-- 32 bit conditional jump must be: load lhs/rhs, Cmp, B cond true, B Al false
test_condjump_32_shape :: IO ()
test_condjump_32_shape = do
    let lt = IR.Types.Label 10
        lf = IR.Types.Label 11
        ir = [ICondJump AST.Operator.Lt intT (VConst 1) (VConst 2) lt lf, ILabel lt, IReturn Nothing, ILabel lf, IReturn Nothing]
        asm = fst (genFunction [] ir 0)

    assert "ICondJump 32 emits Cmp; B(Lt) true; B Al false" $
        subseq
        [\x -> case x of Cmp R0 (OpReg R1) -> True; _ -> False
        , \x -> case x of B ARM7.Types.Lt l -> l == lt; _ -> False
        , \x -> case x of B Al l -> l == lf; _ -> False] asm

-- 64 bit EQ condjump must compare hi then lo (genCondJump64 Eq pattern)
test_condjump_64_eq_shape :: IO ()
test_condjump_64_eq_shape = do
    let lt = IR.Types.Label 20
        lf = IR.Types.Label 21
        ir =
            [ICondJump AST.Operator.Eq longT (VConst 0) (VConst 0) lt lf
            , ILabel lt
            , IReturn Nothing
            , ILabel lf
            , IReturn Nothing]
        asm = fst (genFunction [] ir 0)

    assert "ICondJump 64 Eq emits hi-compare then lo-compare pattern" $
        subseq
        [\x -> case x of Cmp R1 (OpReg R3) -> True; _ -> False
        , \x -> case x of B Ne l -> l == lf; _ -> False
        , \x -> case x of Cmp R0 (OpReg R2) -> True; _ -> False
        , \x -> case x of B ARM7.Types.Eq l -> l == lt; _ -> False
        , \x -> case x of B Al l -> l == lf; _ -> False] asm

-- ICall must emit Bl name and save return into temp when returnType is non-void
test_call_saves_return :: IO ()
test_call_saves_return = do
    let t0 = Temp 0
        ir = [ICall (Just t0) intT "foo" [(intT, VConst 1)], IReturn (Just (intT, VTemp t0))]
        asm = fst (genFunction [] ir 0)

    assert "call emits Bl foo" $
        containsInOrder (isBl "foo") asm

    assert "call saves return from R0 into temp (a Str R0 after Bl foo)" $
        subseq [isBl "foo", \x -> case x of Str R0 _ -> True; _ -> False] asm

main :: IO ()
main = do
    putStrLn "Running codegen tests..."
    test_return_label_and_function_shell
    test_cast_int_to_long_signext
    test_condjump_32_shape
    test_condjump_64_eq_shape
    test_call_saves_return
    putStrLn "OK"
