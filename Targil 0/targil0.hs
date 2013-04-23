import System.IO  
import Data.List
import Data.Char  
import System.Directory
import Control.Monad
import Control.Applicative
import System.FilePath

{-- --}
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False
 
isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False
 
 {-- to check if they are numeric number -> return true/false--}
isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s

process fileName = do
		inh <- openFile fileName ReadMode
		txt <- hGetContents inh
		print txt
		let txtLines = lines txt
		let firstLine = head txtLines
		outh <- openFile fileName WriteMode
		if isNumeric firstLine 
		then do 
			let firstLine = show ((read firstLine :: Int) + 1)
			hPutStr outh (firstLine ++ "\n" ++ unlines (tail txtLines))
		else do
			hPutStr outh ("1\n" ++ txt)
		writeFile (replaceExtension fileName ".out") txt
		putStr $ unlines (filter (isInfixOf "you") txtLines)
		hClose inh


main = do  
	putStrLn "Please enter directory"
	dir <- getLine 
	files <- getDirectoryContents dir
	mapM_ process $ filter (\fileName -> ((takeExtension fileName) == ".in")) files

		