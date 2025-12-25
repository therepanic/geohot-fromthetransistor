module AST.Type where

data Primitive = Int | Long
data Type
  = PrimitiveType Primitive
  | PointerType Type
