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
        getVarDeclType :: VarDecl -> Type
        getVarDeclType v = case v of
            VarDecl name typ Nothing -> typ
            VarDecl name typ _ -> error ("Incorrect variable " ++ name ++ " type")
        go env (Function typ name args body) =
            let
                argsTypes = map getVarDeclType args
            in
                if Map.member name env then error ("Function with name " ++ name ++ " already exist") else Map.insert name (typ, argsTypes) env
        go env _ = env
    in
        foldl' go Map.empty s
