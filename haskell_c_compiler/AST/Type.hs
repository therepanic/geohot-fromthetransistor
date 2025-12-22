module Type where

data Primitive = Int | Long | Float
data Type
  = PrimitiveType Primitive
  | PointerType Type
