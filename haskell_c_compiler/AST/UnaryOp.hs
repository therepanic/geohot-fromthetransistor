module AST.UnaryOp where

data UnaryOp
  = Neg
  | Pos
  | Not
  deriving (Show, Eq)
