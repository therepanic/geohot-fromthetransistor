module VarDecl where

import Type
import Expression

data VarDecl = VarDecl String Type (Maybe Expression)
