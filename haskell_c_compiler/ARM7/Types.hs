module ARM7.Types where

import IR.Types

data Reg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
    | R8 | R9 | R10 | FP | SP | LR deriving (Show, Eq)

data Cond = Eq | Ne | Lt | Le | Gt | Ge | Al
  | Lo | Hs | Hi | Ls deriving (Show, Eq)

data Mem = Mem Reg Int | MemReg Reg deriving (Show, Eq)

data Operand
  = OpReg Reg
  | OpImm Integer
  deriving (Show, Eq)

data AsmInstr
    = Label Label
    | Mov Cond Reg Operand
    | MovC Cond Reg Operand
    | Ldr Reg Mem
    | LdrLit Reg Integer
    | Asr Reg Reg Operand
    | Str Reg Mem
    | Add Reg Reg Operand
    | Adds Reg Reg Operand
    | Adc Reg Reg Operand
    | Subs Reg Reg Operand
    | Sbc Reg Reg Operand
    | Sub Reg Reg Operand
    | Rsb Reg Reg Operand
    | Rsbs Reg Reg Operand
    | Rsc Reg Reg Operand
    | Orr Reg Reg Operand
    | Mul Reg Reg Reg
    | Cmp Reg Operand
    | B Cond Label
    | Bl String
    | Bx Reg
    | Push [Reg]
    | Pop [Reg]
    deriving (Show, Eq)
