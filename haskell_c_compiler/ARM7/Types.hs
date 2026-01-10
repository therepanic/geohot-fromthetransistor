module ARM7.Types where

import IR.Types

data Reg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
    | R8 | R9 | R10 | FP | SP | LR

data Cond = Eq | Ne | Lt | Le | Gt | Ge | Al
  | Lo | Hs | Hi | Ls

data Mem = Mem Reg Int | MemReg Reg

data Operand
  = OpReg Reg
  | OpImm Integer

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
