import Data.List

isNotInfixOf :: Eq a => [a] -> [a] -> Bool
isNotInfixOf x y = not (isInfixOf x y)

main = do
		txt <- readFile "Main.jack"
		let txtLines = lines txt
		let parsed = map (\currLine -> (takeWhile (isNotInfixOf "//") (words currLine))) txtLines
		let stripped = filter (\x -> x /= "") parsed
		let txt2 = unlines stripped
		putStr txt2