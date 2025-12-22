module Token where

data Pos = Pos
    { line :: Int
    , col  :: Int
    } deriving (Show, Eq)

data Token
    = TokIdent String Pos
    | TokInt Int Pos
    | TokPlus Pos
    | TokMinus Pos
    | TokSlash Pos
    | TokStar Pos
    | TokLParen Pos
    | TokRParen Pos
    | TokSemicolon Pos
    | TokEOF Pos
    deriving (Show, Eq)
