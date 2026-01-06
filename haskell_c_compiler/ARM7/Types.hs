module ARM.Types where

import IR.Types

data Reg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
    | R8 | R9 | R10 | FP | SP | LR

data Cond = Eq | Ne | Lt | Le | Gt | Ge | Al

data Mem = Mem Reg Int | MemReg Reg

data Operand
  = OpReg Reg
  | OpImm Integer

data AsmInstr
    = Label Label
    | Mov Reg Operand
    | Ldr Reg Mem
    | LdrLit Reg Integer
    | Str Reg Mem
    | Add Reg Reg Operand
    | Sub Reg Reg Operand
    | Mul Reg Reg Reg
    | Cmp Reg Operand
    | B Cond  Label
    | Bl String
    | Push [Reg]
    | Pop [Reg]
