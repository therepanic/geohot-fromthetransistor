module Semantic.Scope where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import AST.Type

type FuncSig = (Type, [Type])

type GlobalEnv = Map String FuncSig
type LocalEnv = [Map String Type]
