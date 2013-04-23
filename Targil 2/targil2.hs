import System.IO  
import Data.List
import Control.Monad
import System.Environment
import System.FilePath
import Data.Maybe
import System.Directory

isNotInfixOf :: Eq a => [a] -> [a] -> Bool
isNotInfixOf x y = not (isInfixOf x y)

process_VM file = do
		let projectName = last (splitDirectories (dropFileName file))
		txt <- readFile file
		let txtLines = lines txt
		let	filename = dropExtensions (takeFileName file) {-- for static segment --}
		let parsed = map (\currLine -> vm_parse (takeWhile (isNotInfixOf "//") (words currLine)) filename (fromJust (elemIndex currLine txtLines))) txtLines
		let stripped = filter (\x -> x /= "") parsed
		appendFile (replaceFileName file (projectName  ++ ".asm")) (unlines stripped)

main = do
		args <- getArgs
		let dir = head args
		canondir <- canonicalizePath dir
		let projectName = last (splitDirectories canondir)
		files <- getDirectoryContents canondir
		let files2 = map (\x -> (canondir </> x)) files
		writeFile ((canondir </> projectName) ++ ".asm") ((unlines (["@256","D=A","@SP","M=D"])) ++ (vm_parse ["call","Sys.init","0"] projectName 0))
		mapM_ process_VM $ filter (\fileName -> ((takeExtension fileName) == ".vm")) files2
		putStrLn (projectName ++ ".asm saved.")

vm_parse :: [String] -> String -> Int -> String
vm_parse ["add"] _ _ = unlines (["@SP","M=M-1","A=M","D=M","@SP","M=M-1","A=M","A=M","D=A+D","@SP","A=M","M=D","@SP","M=M+1"])
vm_parse ["sub"] _ _ = unlines (["@SP","M=M-1","A=M","D=M","@SP","M=M-1","A=M","A=M","D=A-D","@SP","A=M","M=D","@SP","M=M+1"])
vm_parse ["neg"] _ _ = unlines (["@SP","M=M-1","A=M","D=M","D=-D","@SP","A=M","M=D","@SP","M=M+1"])
vm_parse ["eq"] _ vmLineNum = unlines (["@SP","M=M-1","A=M","D=M","@SP","M=M-1","A=M","D=M-D","@IFTRUE_" ++ (show vmLineNum),"D;JEQ","@0","D=A","@END_" ++ (show vmLineNum),"0;JMP","(IFTRUE_" ++ (show vmLineNum) ++ ")","@0","A=A-1","D=A","(END_" ++ (show vmLineNum) ++ ")","@SP","A=M","M=D","@SP","M=M+1"])
vm_parse ["gt"] _ vmLineNum = unlines (["@SP","M=M-1","A=M","D=M","@SP","M=M-1","A=M","D=M-D","@IFTRUE_" ++ (show vmLineNum),"D;JGT","@0","D=A","@END_" ++ (show vmLineNum),"0;JMP","(IFTRUE_" ++ (show vmLineNum) ++ ")","@0","A=A-1","D=A","(END_" ++ (show vmLineNum) ++ ")","@SP","A=M","M=D","@SP","M=M+1"]) 
vm_parse ["lt"] _ vmLineNum = unlines (["@SP","M=M-1","A=M","D=M","@SP","M=M-1","A=M","D=M-D","@IFTRUE_" ++ (show vmLineNum),"D;JLT","@0","D=A","@END_" ++ (show vmLineNum),"0;JMP","(IFTRUE_" ++ (show vmLineNum) ++ ")","@0","A=A-1","D=A","(END_" ++ (show vmLineNum) ++ ")","@SP","A=M","M=D","@SP","M=M+1"]) 
vm_parse ["and"] _ _ = unlines (["@SP","M=M-1","A=M","D=M","@SP","M=M-1","A=M","A=M","D=A&D","@SP","A=M","M=D","@SP","M=M+1"])
vm_parse ["or"] _ _ = unlines (["@SP","M=M-1","A=M","D=M","@SP","M=M-1","A=M","A=M","D=A|D","@SP","A=M","M=D","@SP","M=M+1"])
vm_parse ["not"] _ _ = unlines (["@SP","M=M-1","A=M","D=M","D=!D","@SP","A=M","M=D","@SP","M=M+1"])
vm_parse ["label",x] fileName _ = unlines (["(" ++ x ++ ")"])
vm_parse ["goto",x] fileName _ = unlines (["@" ++ x, "0;JMP"])
vm_parse ["if-goto",x] _ _ =  unlines (["@SP","M=M-1","A=M","D=M","@" ++ x,"D;JNE"])
vm_parse ["function",funcName,k_vars] fileName vmLineNum = (unlines ([ "(" ++ funcName ++ ")"])) ++ pushloop (read k_vars)
vm_parse ["call",funcName,n_args] fileName vmLineNum = unlines (["@returnadd_" ++ fileName ++ "_" ++ (show vmLineNum),"D=A","@SP","A=M","M=D","@SP","M=M+1","@LCL","D=M","@SP","A=M","M=D","@SP","M=M+1","@ARG","D=M","@SP","A=M","M=D","@SP","M=M+1","@THIS","D=M","@SP","A=M","M=D","@SP","M=M+1","@THAT","D=M","@SP","A=M","M=D","@SP","M=M+1","@" ++ n_args,"D=A","@5","D=D+A","@SP","D=M-D","@ARG","M=D","@SP","D=M","@LCL","M=D","@" ++ funcName,"0;JMP","(returnadd_" ++ fileName ++ "_" ++ (show vmLineNum) ++ ")"])
vm_parse ["return"] fileName vmLineNum = unlines (["@LCL","D=M","@5","A=D-A","D=M","@13","M=D","@SP","M=M-1","A=M","D=M","@ARG","A=M","M=D","@ARG","D=M","@SP","M=D+1","@LCL","M=M-1","A=M","D=M","@THAT","M=D","@LCL","M=M-1","A=M","D=M","@THIS","M=D","@LCL","M=M-1","A=M","D=M","@ARG","M=D","@LCL","M=M-1","A=M","D=M","@LCL","M=D","@13","A=M","0;JMP" ])
vm_parse ["pop",x,y] f _ = hack_func_pop x y f
vm_parse ["push",x,y] f _ = hack_func_push x y f
vm_parse _ _ _ = ""

pushloop :: Int -> String
pushloop 0 = ""
pushloop n = (hack_func_push "constant" "0" "") ++ (pushloop (n-1))

hack_func_push :: String -> String -> String -> String
hack_func_push "constant" a _ = unlines (["@" ++ a,"D=A","@SP","A=M","M=D","@SP","M=M+1"])
hack_func_push "local" a _ = unlines (["@" ++ a,"D=A","@LCL","A=M+D","D=M","@SP","A=M","M=D","@SP","M=M+1"])
hack_func_push "argument" a _ = unlines (["@" ++ a,"D=A","@ARG","A=M+D","D=M","@SP","A=M","M=D","@SP","M=M+1"])
hack_func_push "this" a _ = unlines (["@" ++ a,"D=A","@THIS","A=M+D","D=M","@SP","A=M","M=D","@SP","M=M+1"])
hack_func_push "that" a _ = unlines (["@" ++ a,"D=A","@THAT","A=M+D","D=M","@SP","A=M","M=D","@SP","M=M+1"])
hack_func_push "temp" a _ = unlines (["@" ++ a,"D=A","@5","A=A+D","D=M","@SP","A=M","M=D","@SP","M=M+1"])
hack_func_push "pointer" "0" _ = unlines (["@THIS","D=M","@SP","A=M","M=D","@SP","M=M+1"])
hack_func_push "pointer" "1" _ = unlines (["@THAT","D=M","@SP","A=M","M=D","@SP","M=M+1"])
hack_func_push "static" a filename = unlines (["@" ++ filename ++ "." ++ a,"D=M","@SP","A=M","M=D","@SP","M=M+1"])
hack_func_push _ _ _ = ""

hack_func_pop :: String -> String -> String -> String
hack_func_pop "local" a _ = unlines (["@" ++ a,"D=A","@LCL","D=M+D","@SP","M=M-1","A=M","A=M","A=A+D","D=A-D","A=A-D","M=D"])
hack_func_pop "argument" a _ =  unlines (["@" ++ a,"D=A","@ARG","D=M+D","@SP","M=M-1","A=M","A=M","A=A+D","D=A-D","A=A-D","M=D"])
hack_func_pop "this" a _ =  unlines (["@" ++ a,"D=A","@THIS","D=M+D","@SP","M=M-1","A=M","A=M","A=A+D","D=A-D","A=A-D","M=D"])
hack_func_pop "that" a _ =  unlines (["@" ++ a,"D=A","@THAT","D=M+D","@SP","M=M-1","A=M","A=M","A=A+D","D=A-D","A=A-D","M=D"])
hack_func_pop "temp" a _ = unlines (["@" ++ a,"D=A","@5","D=D+A","@SP","M=M-1","A=M","A=M","A=A+D","D=A-D","A=A-D","M=D"])
hack_func_pop "pointer" "0" _ = unlines (["@SP","M=M-1","A=M","D=M","@THIS","M=D"])
hack_func_pop "pointer" "1" _ = unlines (["@SP","M=M-1","A=M","D=M","@THAT","M=D"])
hack_func_pop "static" a filename = unlines (["@SP","M=M-1","A=M","D=M","@" ++ filename ++ "." ++ a,"M=D"])
hack_func_pop _ _ _ = ""