import Data.List
import JackParse
import System.Directory
import System.FilePath
import System.Environment

process_jack file = do
		parsed <- parseFile file
		let newName = replaceExtension file ".vm"
		writeFile newName parsed
		putStrLn (newName ++ " saved.")

main = do
		args <- getArgs
		let arg = head args
		canonarg <- canonicalizePath arg
		isFile <- doesFileExist canonarg
		if (isFile)
			then do {process_jack canonarg}
			else do {	files <- getDirectoryContents canonarg
					;	let files2 = map (\x -> (canonarg </> x)) files
					;	mapM_ process_jack $ filter (\fileName -> ((takeExtension fileName) == ".jack")) files2
					}