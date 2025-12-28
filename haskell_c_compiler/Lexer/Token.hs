module Lexer.Token where

data Pos = Pos
    { line :: Int
    , col  :: Int
    } deriving (Show, Eq)

data Token
    = TokIdent String Pos
    | TokNum String Pos
    | TokPlus Pos
    | TokMinus Pos
    | TokAssign Pos
    | TokSlash Pos
    | TokStar Pos
    | TokAmpersand Pos
    | TokLParen Pos
    | TokRParen Pos
    | TokLBrace Pos
    | TokRBrace Pos
    | TokComma Pos
    | TokSemicolon Pos
    | TokEOF Pos
    deriving (Show, Eq)
