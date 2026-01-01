module IR.Types where

import AST.Type
import AST.Operator
import AST.UnaryOp

newtype Temp  = Temp Int deriving (Show, Eq)
newtype Label = Label Int deriving (Show, Eq)

data Val = VTemp Temp | VConst Integer deriving (Show, Eq)

data Addr = AVar String | ATemp Temp deriving (Show, Eq)

data Instr
    = ILabel Label
    | IMov Temp Val
    | IBin Temp Type Operator Val Val
    | IAddrOf Temp String
    | ILoad Temp Type Addr
    | IStore Addr Type Val
    | ICondJump Operator Type Val Val Label Label
    | ICast Temp Type Val
    | IUnaryOp Temp Type UnaryOp Val
    | IJump Label
    | ICall (Maybe Temp) String [Val]
    | IReturn (Maybe (Type, Val))
    deriving (Show, Eq)
