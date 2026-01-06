module ARM.Codegen where

import ARM.Frame
import ARM.Types
import AST.Type
import Data.Bits
import IR.Types

genInstr :: Frame -> Instr -> [AsmInstr]

genInstr _ (ILabel l) = [ARM.Types.Label l]

genInstr f (IMov t ty val) =
    case ty of
        PrimitiveType Long ->
            let
                loadInstrs = loadVal64 f R0 R1 val
            in
                loadInstrs ++ storeTemp64 f R0 R1 t
        _ ->
            let
                loadInstrs = loadVal32 f R0 val
            in
                loadInstrs ++ storeTemp32 f R0 t

genInstr f (ILoad t ty addr) =
    case ty of
        PrimitiveType Long ->
            let
                loadInstrs = loadFromAddr64 f R0 R1 R2 addr
            in
                loadInstrs ++ storeTemp64 f R0 R1 t
        _ ->
            let
                loadInstrs = loadFromAddr32 f R0 R1 addr
            in
                loadInstrs ++ storeTemp32 f R0 t

-- =============================
-- Helpers
-- =============================

loadVal32 :: Frame -> Reg -> Val -> [AsmInstr]
loadVal32 f r (VTemp t) = [Ldr r (tempAddr f t)]
loadVal32 f r (VConst n) = [LdrLit r n]

loadVal64 :: Frame -> Reg -> Reg -> Val -> [AsmInstr]
loadVal64 f r1 r2 (VTemp t) =
    case tempAddr f t of
        Mem base off -> [Ldr r1 (Mem base off), Ldr r2 (Mem base (off + 4))]
        MemReg _ -> error "loadVal64: MemReg not expected"
loadVal64 f r1 r2 (VConst n) =
    let
        lo = n .&. 0xFFFFFFFF
        hi = (n `shiftR` 32) .&. 0xFFFFFFFF
    in
        [LdrLit r1 lo, LdrLit r2 hi]

storeTemp32 :: Frame -> Reg -> Temp -> [AsmInstr]
storeTemp32 f r t = [Str r (tempAddr f t)]

storeTemp64 :: Frame -> Reg -> Reg -> Temp -> [AsmInstr]
storeTemp64 f r1 r2 t =
    case tempAddr f t of
        Mem base off -> [Str r1 (Mem base off), Str r2 (Mem base (off + 4))]
        MemReg _ -> error "storeTemp64: MemReg not expected"

loadFromAddr32 :: Frame -> Reg -> Reg -> Addr -> [AsmInstr]
loadFromAddr32 f r _ (AVar name) = [Ldr r (varAddr f name)]
loadFromAddr32 f r rptr (ATemp t) = [Ldr rptr (tempAddr f t), Ldr r (MemReg rptr)]

loadFromAddr64 :: Frame -> Reg -> Reg -> Reg -> Addr -> [AsmInstr]
loadFromAddr64 f r1 r2 _ (AVar name) =
    case varAddr f name of
        Mem base off -> [Ldr r1 (Mem base off), Ldr r2 (Mem base (off + 4))]
        MemReg _ -> error "loadVal64: MemReg not expected"
loadFromAddr64 f r1 r2 rptr (ATemp t) =
    [
    Ldr rptr (tempAddr f t)
    , Ldr r1 (MemReg rptr)
    , Ldr r2 (Mem rptr 4)
    ]
