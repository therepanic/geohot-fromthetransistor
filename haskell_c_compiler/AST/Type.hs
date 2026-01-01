module AST.Type where

data Primitive = Int | Long | Void
  deriving (Show, Eq)
data Type
  = PrimitiveType Primitive
  | PointerType Type
  deriving (Show, Eq)
