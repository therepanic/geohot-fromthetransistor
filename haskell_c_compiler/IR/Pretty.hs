module IR.Pretty where

import AST.Operator
import AST.Type
import AST.UnaryOp
import IR.Types

printIR :: [Instr] -> String
printIR instrs = unlines (map printInstr instrs)

printInstr :: Instr -> String
printInstr (ILabel l) = printLabel l ++ ":"
printInstr (IMov t v) = "   " ++ printTemp t ++ " = mov " ++ printVal v
printInstr (IBin t ty op v1 v2) = " " ++ printTemp t ++ " = " 
    ++ printOp op ++ "." ++ printType ty
    ++ " " ++ printVal v1 ++ " " ++ printVal v2
printInstr (IUnaryOp t ty u v) =
    "   " ++ printTemp t ++ " = "
    ++ printUnary u ++ "." ++ printType ty
    ++ " " ++ printVal v

printInstr (IAddrOf t name) =
    "   " ++ printTemp t ++ " = addrof [" ++ name ++ "]"

printInstr (ILoad t ty addr) =
    "   " ++ printTemp t ++ " = load." ++ printType ty
    ++ " " ++ printAddr addr

printInstr (IStore addr ty v) =
    "   store." ++ printType ty
    ++ " " ++ printAddr addr ++ ", " ++ printVal v

printInstr (ICast t ty v) =
    "   " ++ printTemp t ++ " = cast." ++ printType ty
    ++ " " ++ printVal v

printInstr (ICondJump op ty v1 v2 lt lf) =
    "   cjump." ++ printOp op ++ "." ++ printType ty
    ++ " " ++ printVal v1 ++ " " ++ printVal v2
    ++ " " ++ printLabel lt ++ " " ++ printLabel lf

printInstr (IJump l) =
    "   jump " ++ printLabel l

printInstr (ICall Nothing name args) =
    "   call " ++ name ++ "(" ++ printArgs args ++ ")"

printInstr (ICall (Just t) name args) =
    "   " ++ printTemp t ++ " = call "
    ++ name ++ "(" ++ printArgs args ++ ")"

printInstr (IReturn Nothing) =
    "   return"

printInstr (IReturn (Just (ty, v))) =
    "   return." ++ printType ty ++ " " ++ printVal v

printTemp :: Temp -> String
printTemp (Temp n) = "t" ++ show n

printLabel :: Label -> String
printLabel (Label n) = "L" ++ show n

printVal :: Val -> String
printVal (VTemp t) = printTemp t
printVal (VConst n) = "#" ++ show n

printAddr :: Addr -> String
printAddr (AVar name) = "[" ++ name ++ "]"
printAddr (ATemp t) = "[" ++ printTemp t ++ "]"

printType :: Type -> String
printType = show

printOp :: Operator -> String
printOp = show

printUnary :: UnaryOp -> String
printUnary = show

printArgs :: [Val] -> String
printArgs [] = ""
printArgs [v] = printVal v
printArgs (v:vs) = printVal v ++ ", " ++ printArgs vs
