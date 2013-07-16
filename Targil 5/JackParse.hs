module JackParse where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.List as List
import qualified Data.Map as Map
import Data.IORef
import Data.Char
import Data.Maybe	

listIndexer :: [(String,String)] -> String -> [(String, (String, String, Int))]
listIndexer l s = List.map (\(x,y) -> (x,(y,s,(fromJust(List.elemIndex (x,y) l))))) l

vmVar (c,m) varName	= ((\(x,y,z) -> (y ++ " " ++ show z)) (fromJust(List.lookup varName (c ++ m))))
vmVarMaybe (c,m) varName	= (List.lookup varName (c ++ m))
vmString (xs)	= unlines (["push constant " ++ show (length xs),"call String.new 1"] ++ (List.map (\x -> "push constant " ++ show (ord x) ++ "\ncall String.appendChar 2") xs))
		
--- Lexer ---
jackDef = emptyDef{ Token.commentStart = "/*"
				  , Token.commentEnd = "*/"
				  , Token.commentLine = "//"
				  , Token.identStart = letter <|> char '_'
				  , Token.identLetter = alphaNum <|> char '_'
				  , Token.opLetter = oneOf "+-*/&|<>=~."
				  , Token.reservedOpNames = ["+","-","*","/","&","|","<",">","=","~","."]
				  , Token.reservedNames = ["class", "constructor","function","method","field","static",
										   "var","int","char","boolean","void","true","false","null","this",
										   "let","do","if","else","while","return"]
				  , Token.caseSensitive  = True
				  }
				  
lexer = Token.makeTokenParser jackDef

identifier 		= Token.identifier 		lexer -- parses an identifier
reserved   		= Token.reserved   		lexer -- parses a reserved name
reservedOp 		= Token.reservedOp 		lexer -- parses an operator
parens     		= Token.parens     		lexer -- parses surrounding parenthesis:
											  --   parens p
											  -- takes care of the parenthesis and
											  -- uses p to parse what's inside them
integer    		= Token.integer    		lexer -- parses an integer
semi       		= Token.semi       		lexer -- parses a semicolon
whiteSpace 		= Token.whiteSpace 		lexer -- parses whitespace
squares	   		= Token.squares    		lexer -- parses square brackets
braces	   		= Token.braces     		lexer -- parses curly brackets
commaSep   		= Token.commaSep   		lexer -- parses a comma seperated list
commaSep1  		= Token.commaSep1  		lexer -- parses a comma seperated list
stringLiteral 	= Token.stringLiteral  	lexer -- parses a literal string
symbol	   		= Token.symbol 	  		lexer -- parses a symbol
dot 	   		= Token.dot  	  		lexer -- parses a dot 
natural 		= Token.natural 		lexer -- parses natural numbers

whileParser = whiteSpace >> jackClass
--function part--
jackClass  = do {	reserved "class"
				;	c <- className
				;	symbol "{"
				;	cvd <- many (classVarDec [] [])
				;	let newStaticList = (listIndexer (List.concat (List.map fst cvd)) "static")
				;	let newFieldList = (listIndexer (List.concat (List.map snd cvd)) "this")
				;	sd <- many (subroutineDec c (newStaticList,newFieldList))
				;	symbol "}"
				;	return (unlines sd)
				}
				
classVarDec	staticList fieldList = 	try(do {reserved "static"
										;	t <- jackType
										;	v <- commaSep1 varName
										;	semi
										;	let newStaticList = staticList `List.union` (zip v (repeat t)) 
										;	return (newStaticList,fieldList)
										})
								<|>	try(do {reserved "field"
										;	t <- jackType
										;	v <- commaSep1 varName
										;	semi
										;	let newFieldList = fieldList `List.union` (zip v (repeat t))
										;	return (staticList,newFieldList)
										})

jackType = 	try(do {reserved "int"; return ("int")})
		<|>	try(do {reserved "char"; return ("char")})
		<|>	try(do {reserved "boolean"; return ("boolean")})
		<|>	try(do {c <- className; return c})
		
subroutineDec thisClassName (staticList,fieldList) =	try(do{	reserved "constructor"
							;	t  <- subroutineDecType 
							;	sn <- subroutineName
							;	pl <- parens (parameterList [])
							;	let argMap = (listIndexer pl "argument")
							;	let classMap = staticList ++ fieldList
							;	sb <- subroutineBody [] thisClassName classMap argMap
							;	let methodMap = (snd sb)
							;	let sb2 = (fst sb)
							;	if thisClassName == "Main"
								  then return (unlines ["function " ++ thisClassName ++ "." ++ sn ++ " "++ show ((length methodMap)-(length argMap)),
														"push constant " ++ show (length fieldList),
														"call Memory.alloc 1",
														"pop pointer 0",
														sb2])
								  else return (unlines ["function " ++ thisClassName ++ "." ++ sn ++ " "++ show ((length methodMap)-(length argMap)),
														"push constant " ++ show (length fieldList),
														"call Memory.alloc 1",
														"pop pointer 0",
														sb2])
							})
						<|>	try(do{	reserved "function"
							;	t  <- subroutineDecType
							;	sn <- subroutineName
							;	pl <- parens (parameterList [])
							;	let argMap = (listIndexer pl "argument")
							;	let classMap = staticList ++ fieldList
							;	sb <- subroutineBody [] thisClassName classMap argMap
							;	let methodMap = (snd sb)
							;	let sb2 = (fst sb)
							;	return (unlines ["function " ++ thisClassName ++ "." ++ sn ++ " " ++ show ((length methodMap)-(length argMap)),sb2])
							})
						<|>	try(do{	reserved "method"
							;	t  <- subroutineDecType
							;	sn <- subroutineName
							;	pl <- parens (parameterList [("this", thisClassName)])
							;	let argMap = (listIndexer pl "argument")
							;	let classMap = staticList ++ fieldList
							;	sb <- subroutineBody [] thisClassName classMap argMap
							;	let methodMap = (snd sb)
							;	let sb2 = (fst sb)
							;	if thisClassName == "Main"
								  then return (unlines ["function " ++ thisClassName ++ "." ++ sn ++ " " ++ show ((length methodMap)-(length argMap)),sb2])
								  else return (unlines ["function " ++ thisClassName ++ "." ++ sn ++ " " ++ show ((length methodMap)-(length argMap)),
														"push " ++ (vmVar (classMap,methodMap) "this"),
														"pop pointer 0",
														sb2])
							})
				
subroutineDecType =	try(do {reserved "void"; return ("void")})
				<|>	try(do {t <- jackType; return t})
				
parameterList argMap 	= do	{	p <- commaSep (param argMap)
								;	let newArgMap = argMap ++ (List.concat p)
								; 	return (newArgMap)
								}
									
param argMap 			= do 	{	t <- jackType 
								; 	v <- varName
								;	let newArgMap = [(v,t)]
								; 	return (newArgMap)
								}

subroutineBody varMap thisClassName	classMap argMap = do	{symbol "{"
								;	v <- many (varDec varMap)
								;	let varMap = (listIndexer (List.concat v) "local")
								;	let methodMap = argMap ++ varMap
								;	s <- (statements thisClassName (classMap,methodMap))
								;	symbol "}"
								;	return (s,methodMap)
								}

varDec varMap 			= do 	{	reserved "var"
								;	t <- jackType
								;	v <- commaSep1 varName
								;	let newVarMap = varMap `List.union` (zip v (repeat t))
								;	semi
								;	return (newVarMap)
								}
				
jackOp = 	try(do{ reservedOp "+"; return ("add")})
		<|> try(do{ reservedOp "-"; return ("sub")})
		<|> try(do{ reservedOp "*"; return ("call Math.multiply 2")})
		<|> try(do{ reservedOp "/"; return ("call Math.divide 2")})
		<|> try(do{ reservedOp "&"; return ("and")})
		<|> try(do{ reservedOp "|"; return ("or")})
		<|> try(do{ reservedOp "<"; return ("lt")})
		<|> try(do{ reservedOp ">"; return ("gt")})
		<|> try(do{ reservedOp "="; return ("eq")})

unaryOp = try(do{ reservedOp  "-"; return ("neg")})
	  <|> try(do{ reservedOp  "~"; return ("not")})
			
jackExpression thisClassName maps cmd = try(do	{	t1 <- jackTerm thisClassName maps "push"
												; 	o  <- jackOp
												; 	t2 <- jackTerm thisClassName maps "push"
												; 	return (unlines [t1,t2,o])
												})
									<|> try(do	{	t  <- jackTerm thisClassName maps cmd; return t})

integerConstant = do{i <- natural; 	return ("constant " ++ show i)}

stringConstant = do{s <- stringLiteral; return (vmString s)}

jackTerm thisClassName maps cmd =  	try(do{	t <- integerConstant; return (cmd ++ " " ++ t)})
								<|> try(do{	t <- subroutineCall thisClassName maps; return t})
								<|> try(do{ v <- varName
									;	if (isJust (vmVarMaybe maps v))
											then do {	e <- squares (jackExpression thisClassName maps "push")
													;	return (unlines [	e,
																			"push " ++ vmVar maps v,
																			"add",
																			"pop pointer 1",
																			"push that 0"])
													}
											else fail "this is a jackTerm varName fail"
									})
									<|>	try(do{	v <- varName
									;	if (isJust (vmVarMaybe maps v))
											then return (cmd ++ " " ++ vmVar maps v)
											else fail "this is a jackTerm varName fail"
									})
								<|> try(do{ e <- parens (jackExpression thisClassName maps cmd); return e})
								<|> try(do{	t <- stringConstant; return t})
								<|> try(do{	t <- keywordConstant; return (cmd ++ " " ++ t)})
								<|> try(do{	o <- unaryOp
										;	t <- jackTerm thisClassName maps cmd
										; 	return (unlines [t,o])
										})

subroutineCall thisClassName maps = try(do {s <- subroutineName
										;	e <- parens (expressionList thisClassName maps "push")
										;	let num_e = show (read (snd e) + 1)
										;	let new_e = (fst e)
										;	return (unlines ["push pointer 0",new_e,"call " ++ thisClassName ++ "." ++ s ++ " " ++ num_e])
										})
							<|>		try(do {v <- varName
									;	if (isJust (vmVarMaybe maps v))
											then do {dot
											;	s <- subroutineName
											;	e <- parens (expressionList thisClassName maps "push")
											;	let num_e = show (read (snd e) + 1)
											;	let new_e = (fst e)
											;	return (unlines ["push " ++ (vmVar maps v),new_e,"call " ++ (\(x,y,z) -> (x))(fromJust (vmVarMaybe maps v)) ++ "." ++ s ++ " " ++ num_e])
											}
											else fail "this is a subroutineCall varName fail"
									})
							<|> 	try(do {c <- className
									;	dot
									;	s <- subroutineName
									;	e <- parens (expressionList thisClassName maps "push")
									;	let num_e = (snd e)
									;	let new_e = (fst e)
									;	return (unlines [new_e,"call " ++ c ++ "." ++ s ++ " " ++ num_e])
									})
					
expressionList thisClassName maps cmd	= do{ l <- (commaSep (jackExpression thisClassName maps cmd)); return (unlines l, show (length l))}

className 		= try(do{ i <- identifier ; return i})
subroutineName	= try(do{ i <- identifier ; return i})
varName 		= try(do{ i <- identifier ; return i})

keywordConstant =	try(do{ reserved "true"; return "constant 0\nnot"})
				<|> try(do{ reserved "false"; return "constant 0"})
				<|> try(do{ reserved "null"; return "constant 0"})
				<|> try(do{ reserved "this"; return "pointer 0"})

statements thisClassName maps  = do{ x <- many1 (statement thisClassName maps);	 return (unlines x)}

statement thisClassName maps  =	try(letStmt 	thisClassName maps)
							<|> try(ifStmt 		thisClassName maps)
							<|> try(whileStmt 	thisClassName maps)
							<|> try(doStmt 	   	thisClassName maps)
							<|> try(returnStmt 	thisClassName maps)

letStmt thisClassName maps =	try (do {	reserved "let"
										;	v <- varName
										;	e1 <- squares (jackExpression thisClassName maps "push")
										;	reservedOp "="
										;	e2 <- jackExpression thisClassName maps "push"
										;	semi
										;	return (unlines [	e1,
																"push " ++ vmVar maps v,
																"add",
																e2,
																"pop temp 0",
																"pop pointer 1",
																"push temp 0",
																"pop that 0"])
										})
								<|>	try(do {	reserved "let"
											;	v <- varName
											;	reservedOp "="
											;	e <- jackExpression thisClassName maps "push"
											;	semi
											;	return (unlines [e,"pop " ++ vmVar maps v])
											})

ifStmt thisClassName maps =  try(do{	reserved "if"
										;	e  <- parens (jackExpression thisClassName maps "push")
										;	s1 <- braces (statements 	 thisClassName maps )
										;	reserved "else"
										;	s2 <- braces (statements thisClassName maps )
										;	p <- getPosition
										;	return (unlines[e,
															"if-goto IF_TRUE" 	++ show (sourceLine p),
															"goto IF_FALSE" 	++ show (sourceLine p),
															"label IF_TRUE"		++ show (sourceLine p),
															s1,
															"goto IF_END"		++ show (sourceLine p),
															"label IF_FALSE"	++ show (sourceLine p),
															s2,
															"label IF_END" 	 	++ show (sourceLine p)])
										})
									<|>	try(do{	reserved "if"
										;	e <- parens (jackExpression thisClassName maps "push")
										;	s <- braces (statements 	thisClassName maps)
										;	p <- getPosition
										;	return (unlines[e,
															"if-goto IF_TRUE" 	++ show (sourceLine p),
															"goto IF_FALSE" 	++ show (sourceLine p),
															"label IF_TRUE" 	++ show (sourceLine p),
															s,
															"label IF_FALSE" 	++ show (sourceLine p)])
										})
			
whileStmt thisClassName maps =	do{	reserved "while"
										;	e <- parens (jackExpression thisClassName maps "push")
										;	s <- braces (statements 	thisClassName maps)
										;	p <- getPosition
										;	return (unlines["label WHILE_EXP" 	++ show (sourceLine p),
															e,
															"not",
															"if-goto WHILE_END" ++ show (sourceLine p),
															s,
															"goto WHILE_EXP"	++ show (sourceLine p),
															"label WHILE_END" 	++ show (sourceLine p)])
										}

doStmt thisClassName maps = 	do{	reserved "do"
								;	s <- subroutineCall thisClassName maps
								;	semi
								;	return (unlines[s,"pop temp 0"])
								}

returnStmt thisClassName maps = try(do{	try(reserved "return")
									;	e <- try(jackExpression thisClassName maps "push")
									;	try(semi)
									;	return (unlines[e,"return"])
									})
								<|> try(do{	reserved "return"
									;	semi
									;	return (unlines["push constant 0","return"])
									})

				
stripExtraLineBr :: String -> String
stripExtraLineBr [] = []
stripExtraLineBr ('\n':'\n':xs) = stripExtraLineBr ('\n':xs)
stripExtraLineBr (x:xs) = x : stripExtraLineBr xs
				
parseString :: String -> IO String
parseString str = do 	{	case parse whileParser  "" str of
								Left e  -> error $ show e
								Right r -> return (stripExtraLineBr r)
						}

parseFile :: String -> IO String
parseFile file = do {	program  <- readFile file
					;   case parse whileParser "" program of
						   Left e  -> print e >> fail "parse error"
						   Right r -> return (stripExtraLineBr r)
					}
