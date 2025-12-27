module Semantic.Scope where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import AST.Type

type GlobalEnv = Map String Type
type LocalEnv = [Map String Type]
