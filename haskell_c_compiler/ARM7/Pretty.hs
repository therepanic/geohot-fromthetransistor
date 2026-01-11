module ARM7.Pretty where

import ARM7.Types
import IR.Types
import Data.List (intercalate)

prettyProgram :: [AsmInstr] -> String
prettyProgram =
    unlines . map prettyInstr

prettyInstr :: AsmInstr -> String
prettyInstr instr =
    case instr of
        ARM7.Types.Label l ->
            prettyLabel l ++ ":"

        Mov c rd op ->
            instr3 "mov" c rd op

        MovC c rd op ->
            instr3 "movc" c rd op

        Ldr rd mem ->
            "ldr " ++ prettyReg rd ++ ", " ++ prettyMem mem

        LdrLit rd imm ->
            "ldr " ++ prettyReg rd ++ ", =" ++ show imm

        Str rs mem ->
            "str " ++ prettyReg rs ++ ", " ++ prettyMem mem

        Add rd rn op ->
            instr4 "add" rd rn op

        Adds rd rn op ->
            instr4 "adds" rd rn op

        Adc rd rn op ->
            instr4 "adc" rd rn op

        Sub rd rn op ->
            instr4 "sub" rd rn op

        Subs rd rn op ->
            instr4 "subs" rd rn op

        Sbc rd rn op ->
            instr4 "sbc" rd rn op

        Rsb rd rn op ->
            instr4 "rsb" rd rn op

        Rsbs rd rn op ->
            instr4 "rsbs" rd rn op

        Rsc rd rn op ->
            instr4 "rsc" rd rn op

        Orr rd rn op ->
            instr4 "orr" rd rn op

        Mul rd rn rm ->
            "mul " ++ intercalate ", "
                [ prettyReg rd
                , prettyReg rn
                , prettyReg rm
                ]

        Asr rd rn op ->
            "asr " ++ intercalate ", "
                [ prettyReg rd
                , prettyReg rn
                , prettyOperand op
                ]

        Cmp r op ->
            "cmp " ++ prettyReg r ++ ", " ++ prettyOperand op

        B c l ->
            "b" ++ prettyCond c ++ " " ++ prettyLabel l

        Bl name ->
            "bl " ++ name

        Bx r ->
            "bx " ++ prettyReg r

        Push regs ->
            "push {" ++ prettyRegList regs ++ "}"

        Pop regs ->
            "pop {" ++ prettyRegList regs ++ "}"

instr3 :: String -> Cond -> Reg -> Operand -> String
instr3 name cond rd op =
    name ++ prettyCond cond ++ " "
        ++ prettyReg rd ++ ", "
        ++ prettyOperand op

instr4 :: String -> Reg -> Reg -> Operand -> String
instr4 name rd rn op =
    name ++ " "
        ++ prettyReg rd ++ ", "
        ++ prettyReg rn ++ ", "
        ++ prettyOperand op

prettyOperand :: Operand -> String
prettyOperand op =
    case op of
        OpReg r -> prettyReg r
        OpImm i -> "#" ++ show i

prettyMem :: Mem -> String
prettyMem mem =
    case mem of
        Mem r off ->
            "[" ++ prettyReg r ++ ", #" ++ show off ++ "]"
        MemReg r ->
            "[" ++ prettyReg r ++ "]"

prettyReg :: Reg -> String
prettyReg r =
    case r of
        R0 -> "r0"
        R1 -> "r1"
        R2 -> "r2"
        R3 -> "r3"
        R4 -> "r4"
        R5 -> "r5"
        R6 -> "r6"
        R7 -> "r7"
        R8 -> "r8"
        R9 -> "r9"
        R10 -> "r10"
        FP -> "fp"
        SP -> "sp"
        LR -> "lr"

prettyRegList :: [Reg] -> String
prettyRegList =
    intercalate ", " . map prettyReg

prettyLabel :: Label -> String
prettyLabel (IR.Types.Label n) =
    "L" ++ show n

prettyCond :: Cond -> String
prettyCond c =
    case c of
        Al -> ""
        Eq -> "eq"
        Ne -> "ne"
        Lt -> "lt"
        Le -> "le"
        Gt -> "gt"
        Ge -> "ge"
        Lo -> "lo"
        Hs -> "hs"
        Hi -> "hi"
        Ls -> "ls"
