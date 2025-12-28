module AST.Type where

data Primitive = Int | Long | Void
data Type
  = PrimitiveType Primitive
  | PointerType Type
