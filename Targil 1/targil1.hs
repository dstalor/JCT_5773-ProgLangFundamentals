import System.IO  
import Data.List
import Control.Monad
import System.Environment
import System.FilePath
import Data.Maybe

main = do
		args <- getArgs
		let file = head args
		txt <- readFile file
		let txtLines = lines txt
		let	filename = dropExtension (takeFileName file) {-- for static segment --}
		let parsed = map (\currLine -> vm_parse (words currLine) filename (fromJust (elemIndex currLine txtLines))) txtLines
		writeFile (replaceExtension file ".asm") (unlines parsed)
		putStrLn ((replaceExtension file ".asm") ++ " saved.")

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
vm_parse ["label",x] _ _ = unlines (["(" ++ x ++ ")"])
vm_parse ["goto",x] _ _ = unlines (["@" ++ x, "0;JMP"])
vm_parse ["if-goto",x] _ _ =  unlines (["@SP","M=M-1","A=M","D=M","@" ++ x,"D;JNE"])
vm_parse ["function"] f vmLineNum = "TO BE IMPLEMENTED"
vm_parse ["call"] f vmLineNum = "TO BE IMPLEMENTED"
vm_parse ["return"] f vmLineNum = "TO BE IMPLEMENTED"
vm_parse ["pop",x,y] f _ = hack_func_pop x y f
vm_parse ["push",x,y] f _ = hack_func_push x y f
vm_parse _ _ _ = ""

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