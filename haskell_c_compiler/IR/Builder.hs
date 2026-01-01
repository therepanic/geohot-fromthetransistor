module Builder where

import IR.Types

data Builder = Builder
    {
        nextTemp :: Int
        , nextLabel :: Int
        , code :: [Instr]
    }

freshTemp :: Builder -> (Builder, Temp)
freshTemp b = (b {nextTemp = nextTemp b + 1}, Temp (nextTemp b))

freshLabel :: Builder -> (Builder, Label)
freshLabel b = (b {nextLabel = nextLabel b + 1}, Label (nextLabel b))

emit :: Instr -> Builder -> Builder
emit i b = b {code = i : code b}
