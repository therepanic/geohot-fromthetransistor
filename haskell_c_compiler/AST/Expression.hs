module AST.Expression where

import AST.Operator
import AST.Lit
import AST.UnaryOp
import AST.Type

data Expression
    = AddressOf Expression
    | Deref Expression

    | Cast Type Expression

    | Call Expression [Expression]

    | UnaryOp Expression
    | Binary Operator Expression Expression

    | Literal Lit
    | Var String
