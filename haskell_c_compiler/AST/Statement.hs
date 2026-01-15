module AST.Statement where

import AST.Expression
import AST.Type
import AST.VarDecl

data Statement
    = Assign Expression Expression
    | ExprStmt Expression
    | Function Type String [(String, Type)] [Statement]

    | Return (Maybe Expression)

    | If Expression [Statement] [Statement]
    | While Expression [Statement]

    | VarDeclStmt VarDecl
    deriving (Show, Eq)
