module AST.Type where

data Primitive = Int | Long | Void
  deriving (Eq)
data Type
  = PrimitiveType Primitive
  | PointerType Type
  deriving (Eq)

