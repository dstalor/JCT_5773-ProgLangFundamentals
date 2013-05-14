import Data.List
import CommentStrip

main = do
		txt <- readFile "Main.jack"
		
		-- takes txt, turns it into lines, into words, removes everything after we see a //, then puts it all back together again
--		let stripLineComments = unlines (map (\currLine -> unwords (takeWhile (isNotInfixOf "//") (words currLine))) (lines txt)) 
		let stripLineComments = stripComments txt		
		
		let txt2 = stripLineComments
		putStr txt2