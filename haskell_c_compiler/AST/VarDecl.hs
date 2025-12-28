module AST.VarDecl where

import AST.Type
import AST.Expression

data VarDecl = VarDecl String Type (Maybe Expression)
