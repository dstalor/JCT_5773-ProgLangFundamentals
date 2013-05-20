import Data.List
import CommentStrip
import JackParse

main = do
		txt <- readFile "Main.jack"
		
		let commentFree = stripComments txt	
		
		let txtOut = commentFree
		putStr txtOut