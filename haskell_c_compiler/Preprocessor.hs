module Preprocessor where

-- toy preprocessor. very toy because it is breaking strings

stripLineComment :: String -> String
stripLineComment [] = []
stripLineComment ('/':'/':_) = [] 
stripLineComment (c:cs) = c : stripLineComment cs

stripLineComments :: String -> String
stripLineComments s = unlines (map stripLineComment(lines s))

stripBlockComments :: String -> String
stripBlockComments [] = []
stripBlockComments ('/':'*':cs) = stripBlockComments (dropUntil cs)
    where
        dropUntil ('*':'/':cs) = cs
        dropUntil (_:cs) = dropUntil (cs)
        dropUntil [] = []
stripBlockComments (c:cs) = c : stripBlockComments (cs)

collapseSpaces :: String -> String
collapseSpaces s = unwords (words s) 

preprocess :: String -> String

preprocess line = collapseSpaces $ stripLineComments $ stripBlockComments line
