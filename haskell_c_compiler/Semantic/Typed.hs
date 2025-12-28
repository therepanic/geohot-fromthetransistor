module Typed where

import AST.Type
import AST.UnaryOp
import AST.Operator
import AST.Lit

newtype TProgram = TProgram [TFunction]

data TFunction = TFunction
    {
        tfReturnType :: Type
        , tfName       :: String
        , tfParams     :: [(String, Type)]
        , tfBody       :: [TStatement]
    }


data TExpression = TExpression
    {
        texprType :: Type
        , texprNode :: TExpressionNode
    }

data TExpressionNode
    = TAddressOf TExpression
    | TDeref TExpression

    | TCast Type TExpression

    | TCall String [TExpression]

    | TUnaryOp UnaryOp TExpression
    | TBinary Operator TExpression TExpression

    | TLiteral Lit
    | TVar String

data TStatement
    = TAssign TExpression TExpression
    | TExprStmt TExpression

    | TReturn (Maybe TExpression)

    | TIf TExpression [TStatement] [TStatement]
    | TWhile TExpression [TStatement]
    | TVarDecl String Type (Maybe TExpression)

