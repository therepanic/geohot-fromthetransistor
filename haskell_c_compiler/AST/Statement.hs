module Statement where

import Expression
import Type
import VarDecl

data Statement
    = Assign Expression Expression
    | ExprStmt Expression

    | Function Type String [VarDecl] [Statement]

    | Return (Maybe Expression)

    | If Expression [Statement] [Statement]
    | While Expression [Statement]

    | VarDeclStmt VarDecl
