module Expression where

import Operator
import Literal
import UnaryOp
import Type

data Expression
    = AddressOf Expression
    | Deref Expression

    | Cast Type Expression

    | Call String [Expression]

    | UnaryOp UnaryOp Expression
    | Binary Operator Expression Expression

    | Literal Literal
    | Var String
