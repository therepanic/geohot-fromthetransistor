module AST.Lit where

newtype Lit = LNum String
    deriving (Show, Eq)
