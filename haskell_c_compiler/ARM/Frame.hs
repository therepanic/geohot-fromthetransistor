module ARM.Frame where

import qualified Data.Map.Strict as Map

import IR.Types

data Frame = Frame
    {
        varOffset :: Map.Map String Int
        , tempOffset :: Map.Map Temp Int
        , frameSize :: Int
    }
