module Semantic.Collect where

import qualified Data.Map.Strict as Map

import AST.Statement
import AST.VarDecl
import AST.Type
import Semantic.Scope
import Data.List (foldl')

collectFunctions :: [Statement] -> GlobalEnv

collectFunctions s =
    let
        go :: GlobalEnv -> Statement -> GlobalEnv
        go env (Function typ name args body) =
            if Map.member name env
                then error ("Function with name " ++ name ++ " already exist")
                else Map.insert name (typ, gettypes args) env
            where
                gettypes :: [(String, Type)] -> [Type]
                gettypes ((n, t):a) = [y | (x, y) <- a]
        go env _ = env
    in
        foldl' go Map.empty s
