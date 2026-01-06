module ARM.Codegen where

import ARM7.Frame
import ARM7.Types
import AST.Type
import AST.Operator
import Data.Bits
import IR.Types

genInstr :: Frame -> Instr -> Label -> [AsmInstr]

genInstr _ (ILabel l) _ = [ARM7.Types.Label l]

genInstr f (IMov t ty val) _ =
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

genInstr f (ILoad t ty addr) _ =
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

genInstr f (IStore a ty v) _ =
    case ty of
        PrimitiveType Long -> storeInAddr64 f a R0 R1 R2 v
        _ -> storeInAddr32 f a R0 R2 v

genInstr f (IJump l) _ = [B Al l]

genInstr f (IReturn val) rtrn =
    case val of
        Just (t, v) ->
            (case t of
                PrimitiveType Long -> loadVal64 f R0 R1 v
                _ -> loadVal32 f R0 v
            ) ++ [B Al rtrn]
        Nothing -> [B Al rtrn]

genInstr f (IAddrOf t ty name) _ =
    let
        Mem base off = varAddr f name
    in
        [
            Mov R0 (OpReg base),
            LdrLit R1 (fromIntegral off),
            Add R0 R0 (OpReg R1)
        ] ++ storeTemp32 f R0 t

genInstr f (ICondJump op (PrimitiveType Long) lhs rhs lTrue lFalse) _ =
    loadVal64 f R0 R1 lhs ++ loadVal64 f R2 R3 rhs ++ genCondJump64 op lTrue lFalse
genInstr f (ICondJump op _ lhs rhs lTrue lFalse) _ =
    let
        load1 = loadVal32 f R0 lhs
        load2 = loadVal32 f R1 rhs
        cond = relOpToCond op
    in
        load1 ++ load2 ++ [Cmp R0 (OpReg R1), B cond lTrue, B Al lFalse] 

-- =============================
-- Helpers
-- =============================

genCondJump64 :: Operator -> Label -> Label -> [AsmInstr]
genCondJump64 op lTrue lFalse =
    case op of
        AST.Operator.Eq ->
            [
                Cmp R1 (OpReg R3)
                , B Ne lFalse
                , Cmp R0 (OpReg R2)
                , B ARM7.Types.Eq lTrue
                , B Al lFalse
            ]
        AST.Operator.Neq ->
            [
                Cmp R1 (OpReg R3)
                , B Ne lTrue
                , Cmp R0 (OpReg R2)
                , B Ne lTrue
                , B Al lFalse
            ]
        AST.Operator.Lt ->
            [
                Cmp R1 (OpReg R3)
                , B ARM7.Types.Lt lTrue
                , B ARM7.Types.Gt lFalse
                , Cmp R0 (OpReg R2)
                , B Lo lTrue
                , B Al lFalse
            ]
        AST.Operator.Lte ->
            [
                Cmp R1 (OpReg R3)
                , B ARM7.Types.Lt lTrue
                , B ARM7.Types.Gt lFalse
                , Cmp R0 (OpReg R2)
                , B Ls lTrue
                , B Al lFalse
            ]
        AST.Operator.Gt ->
            [
                Cmp R1 (OpReg R3)
                , B ARM7.Types.Gt lTrue
                , B ARM7.Types.Lt lFalse
                , Cmp R0 (OpReg R2)
                , B Hi lTrue
                , B Al lFalse
            ]
        AST.Operator.Gte ->
            [
                Cmp R1 (OpReg R3)
                , B ARM7.Types.Gt lTrue
                , B ARM7.Types.Lt lFalse
                , Cmp R0 (OpReg R2)
                , B Hs lTrue
                , B Al lFalse
            ]

relOpToCond :: Operator -> Cond
relOpToCond op =
    case op of
        AST.Operator.Eq -> ARM7.Types.Eq
        AST.Operator.Neq -> Ne
        AST.Operator.Lt -> ARM7.Types.Lt
        AST.Operator.Lte -> Le
        AST.Operator.Gt -> ARM7.Types.Gt
        AST.Operator.Gte -> Ge

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

storeInAddr32 :: Frame -> Addr -> Reg -> Reg -> Val -> [AsmInstr]
storeInAddr32 f (AVar name) r _ v =
    let
        vaddr = varAddr f name
        linstrs = loadVal32 f r v
    in
        case vaddr of
            Mem base off -> linstrs ++ [Str r (Mem base off)]
            MemReg reg -> linstrs ++ [Str r (MemReg reg)]

storeInAddr32 f (ATemp t) r rptr v =
    let
        instr = Ldr rptr (tempAddr f t)
        sinstr = Str r (MemReg rptr)
    in
        [instr] ++ (case v of
            VTemp t -> [Ldr r (tempAddr f t)]
            VConst n -> [LdrLit r n]
        ) ++ [sinstr]

storeInAddr64 :: Frame -> Addr -> Reg -> Reg -> Reg -> Val -> [AsmInstr]
storeInAddr64 f (AVar name) r1 r2 _ v =
    let
        vaddr = varAddr f name
        linstrs = loadVal64 f r1 r2 v
    in
        case vaddr of
            Mem base off -> linstrs ++ [Str r1 (Mem base off), Str r2 (Mem base (off + 4))]
            MemReg _ -> error "storeInAddr64: MemReg not expected"

storeInAddr64 f (ATemp t) r1 r2 rptr v =
    let
        ptrInstr = Ldr rptr (tempAddr f t)
        instrs = loadVal64 f r1 r2 v
    in
        ptrInstr : instrs ++ [Str r1 (MemReg rptr), Str r2 (Mem rptr 4)]
