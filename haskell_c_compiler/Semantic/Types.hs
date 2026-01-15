module Semantic.Types where

import AST.Type
import AST.UnaryOp
import AST.Operator
import AST.Lit

newtype TProgram = TProgram [TFunction] deriving (Show, Eq)

data TFunction = TFunction
    {
        tfReturnType :: Type
        , tfName :: String
        , tfParams :: [(String, Type)]
        , tfBody :: [TStatement]
    }
    deriving (Show, Eq)


data TExpression = TExpression
    {
        texprType :: Type
        , texprNode :: TExpressionNode
    }
    deriving (Show, Eq)

data TExpressionNode
    = TAddressOf TExpression
    | TDeref TExpression

    | TCast Type TExpression

    | TCall String [TExpression]

    | TUnaryOp UnaryOp TExpression
    | TBinary Operator TExpression TExpression

    | TLiteral Lit
    | TVar String
    deriving (Show, Eq)

data TStatement
    = TAssign TExpression TExpression
    | TExprStmt TExpression

    | TReturn (Maybe TExpression)

    | TIf TExpression [TStatement] [TStatement]
    | TWhile TExpression [TStatement]
    | TVarDecl String Type (Maybe TExpression)
    deriving (Show, Eq)
