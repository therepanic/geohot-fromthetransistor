module ARM7.Frame where

import qualified Data.Map.Strict as Map

import ARM7.Types
import AST.Type
import Data.List (foldl')
import IR.Types

data Frame = Frame
    {
        varOffset :: Map.Map String Int
        , tempOffset :: Map.Map Temp Int
        , frameSize :: Int
    }

buildFrame :: [(String, Type)] -> [Instr] -> Frame
buildFrame args body =
    let
        newFrame = buildArgs args Frame {varOffset = Map.empty, tempOffset = Map.empty, frameSize = 0}
        newFrame1 = buildInstrs body newFrame
    in
        newFrame1 {frameSize = alignUp (frameSize newFrame1) 8}

varAddr :: Frame -> String -> Mem
varAddr f n = case Map.lookup n (varOffset f) of
    Just v -> Mem FP v
    Nothing -> error ("varAddr: unknown var " ++ show n)

tempAddr :: Frame -> Temp -> Mem
tempAddr f n = case Map.lookup n (tempOffset f) of
    Just v -> Mem FP v
    Nothing -> error ("tempAddr: unknown temp " ++ show n)

-- =============================
-- Helpers
-- =============================
buildArgs :: [(String, Type)] -> Frame -> Frame
buildArgs [] f = f
buildArgs ((name, ty):as) f = buildArgs as (allocVar name ty f)

allocVar :: String -> Type -> Frame -> Frame
allocVar name ty f
    | Map.member name (varOffset f) = f
    | otherwise =
        let
            sz = sizeOf ty
            al = alignOf ty
            newS = alignUp (frameSize f + sz) al
            off = -newS
        in
            f {varOffset = Map.insert name off (varOffset f), frameSize = newS}

buildInstrs :: [Instr] -> Frame -> Frame
buildInstrs is f = foldl' (flip step) f is

step :: Instr -> Frame -> Frame
step instr f =
    case instr of
        IMov t ty _ -> allocTemp t ty f
        IBin t ty _ _ _ -> allocTemp t ty f
        IUnaryOp t ty _ _ -> allocTemp t ty f
        ICast t _ ty _ -> allocTemp t ty f
        ILoad t ty addr -> allocTemp t ty (allocVarsInAddr addr ty f)
        IStore addr ty _ -> allocVarsInAddr addr ty f
        IAddrOf t ty name -> allocTemp t (PointerType ty) (allocVar name ty f)
        ICall (Just t) ty _ _ -> allocTemp t ty f
        _ -> f

allocVarsInAddr :: Addr -> Type -> Frame -> Frame
allocVarsInAddr addr ty f =
    case addr of
        AVar name -> allocVar name ty f
        ATemp _   -> f

allocTemp :: Temp -> Type -> Frame -> Frame
allocTemp t ty f
    | Map.member t (tempOffset f) = f
    | otherwise =
        let
            sz = sizeOf ty
            al = alignOf ty
            newS = alignUp (frameSize f + sz) al
            off = -newS
        in
            f {tempOffset = Map.insert t off (tempOffset f), frameSize = newS}
alignUp :: Int -> Int -> Int
alignUp n a = ((n + a - 1) `div` a) * a

sizeOf :: Type -> Int
sizeOf (PointerType _) = 4
sizeOf (PrimitiveType Int) = 4
sizeOf (PrimitiveType Long) = 8
sizeOf _ = error "Unhandled type"

alignOf :: Type -> Int
alignOf (PointerType _) = 4
alignOf (PrimitiveType Int) = 4
alignOf (PrimitiveType Long) = 8
alignOf (PrimitiveType Void) = 1
alignOf _ = error "Unhandled type"
