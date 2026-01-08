module ARM.Codegen where

import ARM7.Frame
import ARM7.Types
import AST.Type
import AST.Operator
import AST.UnaryOp
import Control.Monad.State
import Control.Monad.Reader (ReaderT, runReader, ask, asks)
import Data.Bits
import IR.Types

data CgEnv = CgEnv
    {
        frame :: Frame
        , returnLabel :: Label
    }

type Codegen a = ReaderT CgEnv (State Int) a

freshAsmLabel :: Codegen Label
freshAsmLabel = do
    n <- get
    put (n + 1)
    pure (IR.Types.Label n)

genInstr :: Instr -> Codegen [AsmInstr]

genInstr (ILabel l) = pure [ARM7.Types.Label l]

genInstr (IMov t ty val) = do
    f <- asks frame
    pure $ case ty of
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

genInstr (ILoad t ty addr) = do
    f <- asks frame
    pure $ case ty of
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

genInstr (IStore a ty v) = do
    f <- asks frame
    pure $ case ty of
        PrimitiveType Long -> storeInAddr64 f a R0 R1 R2 v
        _ -> storeInAddr32 f a R0 R2 v

genInstr (IJump l) = pure [B Al l]

genInstr (IReturn val) = do
    f <- asks frame
    rtrn <- asks returnLabel
    pure $ case val of
        Just (t, v) ->
            (case t of
                PrimitiveType Long -> loadVal64 f R0 R1 v
                _ -> loadVal32 f R0 v
            ) ++ [B Al rtrn]
        Nothing -> [B Al rtrn]

genInstr (IAddrOf t _ name) = do
    f <- asks frame
    let Mem base off = varAddr f name
    pure $
        [
            Mov Al R0 (OpReg base),
            LdrLit R1 (fromIntegral off),
            Add R0 R0 (OpReg R1)
        ] ++ storeTemp32 f R0 t

genInstr (ICondJump op (PrimitiveType Long) lhs rhs lTrue lFalse) = do
    f <- asks frame
    pure $ loadVal64 f R0 R1 lhs ++ loadVal64 f R2 R3 rhs ++ genCondJump64 op lTrue lFalse
genInstr (ICondJump op _ lhs rhs lTrue lFalse) = do
    f <- asks frame
    let
        load1 = loadVal32 f R0 lhs
        load2 = loadVal32 f R1 rhs
        cond = relOpToCond op
    pure $ load1 ++ load2 ++ [Cmp R0 (OpReg R1), B cond lTrue, B Al lFalse] 

genInstr (IUnaryOp t ty op v) = do
    f <- asks frame
    case op of
        Pos ->
            case ty of
                PrimitiveType Long -> pure $ loadVal64 f R0 R1 v ++ storeTemp64 f R0 R1 t
                _ -> pure $ loadVal32 f R0 v ++ storeTemp32 f R0 t
        Not ->
            case ty of
                PrimitiveType Long -> pure $ loadVal64 f R0 R1 v
                    ++ [Orr R2 R0 (OpReg R1), Cmp R2 (OpImm 0), Mov ARM7.Types.Eq R0 (OpImm 1), Mov ARM7.Types.Ne R0 (OpImm 0)]
                    ++ storeTemp32 f R0 t
                _ -> pure $ loadVal32 f R0 v
                    ++ [Cmp R0 (OpImm 0), Mov ARM7.Types.Eq R0 (OpImm 1), Mov Ne R0 (OpImm 0)]
                    ++ storeTemp32 f R0 t
        Neg ->
            case ty of
                PrimitiveType Long -> pure $ loadVal64 f R0 R1 v
                    ++ [Rsbs R0 R0 (OpImm 0), Rsc R1 R1 (OpImm 0)]
                    ++ storeTemp64 f R0 R1 t
                _ -> pure $ loadVal32 f R0 v ++ [Rsb R0 R0 (OpImm 0)] ++ storeTemp32 f R0 t

genInstr (ICast t fromType toType v) = do
    f <- asks frame
    case (fromType, toType) of
        (PrimitiveType Int, PrimitiveType Long) -> pure $ loadVal32 f R0 v ++ [Asr R1 R0 (OpImm 31)]
            ++ storeTemp64 f R0 R1 t
        (PrimitiveType Long, PrimitiveType Int) -> pure $ loadVal64 f R0 R1 v ++ storeTemp32 f R0 t
        (PrimitiveType Int, PointerType _) -> pure $ loadVal32 f R0 v ++ storeTemp32 f R0 t
        _ -> pure []

genInstr (IBin t ty op lhs rhs) = do
    f <- asks frame
    case op of
        Plus ->
            case ty of
                PrimitiveType Long -> pure $ loadVal64 f R0 R1 lhs ++ loadVal64 f R2 R3 rhs
                    ++ [Adds R0 R0 (OpReg R2), Adc R1 R1 (OpReg R3)] ++ storeTemp64 f R0 R1 t
                _ -> pure $ loadVal32 f R0 lhs ++ loadVal32 f R1 rhs ++ [Add R0 R0 (OpReg R1)]
                    ++ storeTemp32 f R0 t
        Minus ->
            case ty of
                PrimitiveType Long -> pure $ loadVal64 f R0 R1 lhs ++ loadVal64 f R2 R3 rhs
                    ++ [Subs R0 R0 (OpReg R2), Sbc R1 R1 (OpReg R3)] ++ storeTemp64 f R0 R1 t
                _ -> pure $ loadVal32 f R0 lhs ++ loadVal32 f R1 rhs 
                    ++ [Sub R0 R0 (OpReg R1)] ++ storeTemp32 f R0 t
        AST.Operator.Mul ->
            case ty of
                PrimitiveType Long -> pure $ loadVal64 f R0 R1 lhs ++ loadVal64 f R2 R3 rhs
                    ++ [Bl "__aeabi_lmul"] ++ storeTemp64 f R0 R1 t
                _ -> pure $ loadVal32 f R0 lhs ++ loadVal32 f R1 rhs
                    ++ [ARM7.Types.Mul R0 R0 R1] ++ storeTemp32 f R0 t
        Div -> error "Todo"
        AST.Operator.Gt -> genCmpBin f t ty op lhs rhs
        AST.Operator.Lt -> genCmpBin f t ty op lhs rhs
        AST.Operator.Gte -> genCmpBin f t ty op lhs rhs
        AST.Operator.Lte -> genCmpBin f t ty op lhs rhs
        AST.Operator.Eq -> genCmpBin f t ty op lhs rhs
        AST.Operator.Neq -> genCmpBin f t ty op lhs rhs

-- =============================
-- Helpers
-- =============================

emitBool64 :: Operator -> Label -> Label -> Label -> [AsmInstr]
emitBool64 op lTrue lFalse lEnd = genCondJump64 op lTrue lFalse ++
    [ARM7.Types.Label lTrue
    , Mov Al R0 (OpImm 1)
    , B Al lEnd
    , ARM7.Types.Label lFalse
    , Mov Al R0 (OpImm 0)
    , ARM7.Types.Label lEnd]

emitBool32 :: Cond -> [AsmInstr]
emitBool32 cond = [Mov Al R0 (OpImm 0), MovC cond R0 (OpImm 1)]

genCmpBin :: Frame -> Temp -> Type -> Operator -> Val -> Val -> Codegen [AsmInstr]
genCmpBin f t ty op lhs rhs =
    case ty of
        PrimitiveType Long -> do
            lTrue <- freshAsmLabel
            lFalse <- freshAsmLabel
            lEnd <- freshAsmLabel
            pure $
                loadVal64 f R0 R1 lhs ++
                loadVal64 f R2 R3 rhs ++
                emitBool64 op lTrue lFalse lEnd ++
                storeTemp32 f R0 t
        _ ->
            pure $
                loadVal32 f R0 lhs ++
                loadVal32 f R1 rhs ++
                [Cmp R0 (OpReg R1)]
                ++ emitBool32 (relOpToCond op)
                ++ storeTemp32 f R0 t

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
