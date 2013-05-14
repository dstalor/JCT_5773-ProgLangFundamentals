module CommentStrip where

stripComments :: String -> String
stripComments [] = []
stripComments ('\'':xs)	= '\'' : inSingleString xs
stripComments ('"':xs)	= '"' : inDoubleString xs
stripComments ('/':'/':xs) = inComment xs 
stripComments ('/':'*':xs) = inMultiComment xs
stripComments (x:xs) = x : stripComments xs

inSingleString :: String -> String
inSingleString ('\'':xs) = '\'' : stripComments xs
inSingleString (x:xs) = x : inSingleString xs
inSingleString [] = []

inDoubleString :: String -> String
inDoubleString ('"':xs) = '"' : stripComments xs
inDoubleString (x:xs) = x : inDoubleString xs
inDoubleString [] = []

inComment :: String -> String
inComment ('\n':xs) = stripComments xs
inComment (_:xs) = inComment xs
inComment [] = []

inMultiComment :: String -> String
inMultiComment ('*':'/':xs) = stripComments xs
inMultiComment (_:xs) = inMultiComment xs
inMultiComment [] = []