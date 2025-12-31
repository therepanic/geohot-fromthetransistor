module IR where

newtype Temp  = Temp Int deriving (Show, Eq)
newtype Label = Label String deriving (Show, Eq)

data Val = VTemp Temp | VConst Integer deriving (Show, Eq)

data Addr = AVar String | ATemp Temp deriving (Show, Eq)

data BinOp = Add | Sub | Mul | Div deriving (Show, Eq)
data RelOp = Eq | Ne | Lt | Le | Gt | Ge deriving (Show, Eq)

data Instr
    = ILabel Label
    | IMov Temp Val
    | IBin Temp BinOp Val Val
    | IAddrOf Temp String
    | ILoad Temp Addr
    | IStore Addr Val
    | ICondJump RelOp Val Val Label Label
    | IJump Label
    | ICall (Maybe Temp) String [Val]
    | IReturn (Maybe Val)
    deriving (Show, Eq)
