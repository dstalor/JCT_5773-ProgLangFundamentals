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

mapIndexer m s 	= (Map.fromList (List.map (\(x,y) -> (x,(y,s,(Map.findIndex x m)))) (Map.toList m)))
showMap m 		= (Map.showTreeWith (\k (x,y,z) -> (show k) ++ "|" ++ (show x) ++ "|" ++ (show y) ++ "|" ++ (show z)) False False m)
vmVar (c,m) varName	= ((\(x,y,z) -> (y ++ " " ++ show z)) ((Map.union c m) Map.! varName))
vmVarMaybe (c,m) varName	= (Map.lookup varName (Map.union c m))
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

jackClass  = do {	reserved "class"
				;	c <- className
				;	let staticMap = Map.empty
				;	let fieldMap = Map.empty
				;	symbol "{"
				;	cvd <- many (classVarDec staticMap fieldMap)
				;	let staticMap = (mapIndexer (Map.unions (List.map fst cvd)) "static")
				;	let fieldMap = (mapIndexer (Map.unions (List.map snd cvd)) "this")
				;	sd <- many (subroutineDec c (staticMap,fieldMap))
				;	symbol "}"
				;	return (unlines sd)
				}

classVarDec	staticMap fieldMap = 	do {reserved "static"
				;	t <- jackType
				;	v <- commaSep1 varName
				;	semi
				;	let newStaticMap = (Map.union (Map.fromList (zip v (repeat t))) staticMap)
				;	return (newStaticMap,fieldMap)
				}
			<|>	do {reserved "field"
				;	t <- jackType
				;	v <- commaSep1 varName
				;	semi
				;	let newFieldMap = (Map.union (Map.fromList (zip v (repeat t))) fieldMap)
				;	return (staticMap,newFieldMap)
				}

jackType = 	do {reserved "int"; return ("int")}
		<|>	do {reserved "char"; return ("char")}
		<|>	do {reserved "boolean"; return ("boolean")}
		<|>	do {c <- className; return c}
		
subroutineDec thisClassName (staticMap,fieldMap) =	do{	reserved "constructor"
							;	let argMap = Map.empty
							;	let varMap = Map.empty
							;	t  <- subroutineDecType 
							;	sn <- subroutineName
							;	pl <- parens (parameterList argMap)
							;	let argMap2 = Map.insert " this" thisClassName pl   
							;	let argMap3 = (mapIndexer argMap2 "argument")
							;	let classMap = (Map.union staticMap fieldMap)
							;	sb <- subroutineBody varMap thisClassName classMap argMap3
							;	let methodMap = (snd sb)
							;	let sb2 = (fst sb)
							;	if thisClassName == "Main"
								  then return (unlines ["function " ++ thisClassName ++ "." ++ sn ++ " "++ show ((length(Map.toList methodMap))-(length(Map.toList argMap3))),"push constant " ++ show (length (Map.toList fieldMap)),"call Memory.alloc 1","pop pointer 0",sb2])
								  else return (unlines ["function " ++ thisClassName ++ "." ++ sn ++ " "++ show ((length(Map.toList methodMap))-(length(Map.toList argMap3))),"push constant " ++ show (length (Map.toList fieldMap)),"call Memory.alloc 1","pop pointer 0",sb2])
							}
						<|>	do{	reserved "function"
							;	let argMap = Map.empty
							;	let varMap = Map.empty
							;	t  <- subroutineDecType
							;	sn <- subroutineName
							;	pl <- parens (parameterList argMap)
							;	let argMap2 = Map.insert " this" thisClassName pl
							;	let argMap3 = (mapIndexer argMap2 "argument")
							;	let classMap = (Map.union staticMap fieldMap)
							;	sb <- subroutineBody varMap thisClassName classMap argMap3
							;	let methodMap = (snd sb)
							;	let sb2 = (fst sb)
							;	return (unlines ["function " ++ thisClassName ++ "." ++ sn ++ " " ++ show ((length(Map.toList methodMap))-(length(Map.toList argMap3))),sb2])
							}
						<|>	do{	reserved "method"
							;	let argMap = Map.empty
							;	let varMap = Map.empty
							;	t  <- subroutineDecType
							;	sn <- subroutineName
							;	pl <- parens (parameterList argMap)
							;	let argMap2 = Map.insert " this" thisClassName  pl     
							;	let argMap3 = (mapIndexer argMap2 "argument")
							;	let classMap = (Map.union staticMap fieldMap)
							;	sb <- subroutineBody varMap thisClassName classMap argMap3
							;	let methodMap = (snd sb)
							;	let sb2 = (fst sb)
							;	if thisClassName == "Main"
								  then return (unlines ["function " ++ thisClassName ++ "." ++ sn ++ " " ++ show ((length(Map.toList methodMap))-(length(Map.toList argMap3))),sb2])
								  else return (unlines ["function " ++ thisClassName ++ "." ++ sn ++ " " ++ show ((length(Map.toList methodMap))-(length(Map.toList argMap3))),"push argument 0","pop pointer 0",sb2])
							}
				
subroutineDecType =	try (do {reserved "void"; return ("void")})
				<|>	do {t <- jackType; return t}
				
parameterList argMap 	= do	{	p <- commaSep (param argMap)
								;	let argMap = (Map.unions p)
								; 	return (argMap)
								}
									
param argMap 			= do 	{	t <- jackType 
								; 	v <- varName
								;	let newArgMap = (Map.singleton v t)
								; 	return (newArgMap)
								}

subroutineBody varMap thisClassName	classMap argMap = do	{symbol "{"
								;	v <- many (varDec varMap)
								;	let varMap = (mapIndexer(Map.unions v) "local")
								;	let methodMap = (Map.union argMap varMap)
								;	s <- (statements thisClassName (classMap,methodMap) (0,0))
								;	symbol "}"
								;	return (s,methodMap)
								}

varDec varMap 			= do 	{	reserved "var"
								;	t <- jackType
								;	v <- commaSep1 varName
								;	let newVarMap = (Map.union (Map.fromList (zip v (repeat t))) varMap)
								;	semi
								;	return (newVarMap)
								}
				
jackOp = 	do{ reservedOp "+"; return ("add")}
		<|> do{ reservedOp "-"; return ("sub")}
		<|> do{ reservedOp "*"; return ("call Math.multiply 2")}
		<|> do{ reservedOp "/"; return ("call Math.divide 2")}
		<|> do{ reservedOp "&"; return ("and")}
		<|> do{ reservedOp "|"; return ("or")}
		<|> do{ reservedOp "<"; return ("lt")}
		<|> do{ reservedOp ">"; return ("gt")}
		<|> do{ reservedOp "="; return ("eq")}

unaryOp = do{ reservedOp  "-"; return ("neg")}
	<|> do{ reservedOp  "~"; return ("not")}
			
jackExpression thisClassName maps cmd = try(do	{	t1 <- jackTerm thisClassName maps "push"
										; 	o  <- jackOp
										; 	t2 <- jackTerm thisClassName maps "push"
										; 	return (unlines [t1,t2,o])
										})
							<|> try (do	{	t  <- jackTerm thisClassName maps cmd; return t})

integerConstant = do{i <- natural; 	return ("constant " ++ show i)}

stringConstant = try (do{s <- try (stringLiteral); return (vmString s)})

jackTerm thisClassName maps cmd =  do	{ 	t <- integerConstant; return (cmd ++ " " ++ t)}
		<|> do	{ 	t <- stringConstant; return t}
		<|> try (do	{ 	t <- try (keywordConstant); return (cmd ++ " " ++ t)})
		<|> try (do{ v <- try (varName)
				;	 e <- try (squares (jackExpression thisClassName maps "push"))
				;	 return (unlines ["push " ++ vmVar maps v,e,"add","pop pointer 1","push that 0"])
				})
		<|> try (do	{ 	t <- try (subroutineCall thisClassName maps); return t})
		<|> try (do	{ 	o <- try (unaryOp)
					;	t <- try (jackTerm thisClassName maps cmd)
					; 	return (unlines [t,o])
					})
		<|> try (do{ e <- try (parens (jackExpression thisClassName maps cmd)); return e})

		<|> try (do{ t <- try(varName); return (cmd ++ " " ++ vmVar maps t)})

subroutineCall thisClassName maps = try (do {s <- subroutineName
					;	e <- parens (expressionList thisClassName maps "push")
					;	let num_e = show (read (snd e) + 1)
					;	let new_e = (fst e)
					;	return (unlines ["push pointer 0",new_e,"call " ++ thisClassName ++ "." ++ s ++ " " ++ num_e,"pop temp 0"])
					})
			<|>		try (do {v <- try (varName)
					;	if (isJust (vmVarMaybe maps v))
							then do {
							;	try (dot)
							;	s <- subroutineName
							;	e <- parens (expressionList thisClassName maps "push")
							;	let num_e = show (read (snd e) + 1)
							;	let new_e = (fst e)
							;	return (unlines ["push local 0",new_e,"call " ++ (\(x,y,z) -> (x))(fromJust (vmVarMaybe maps v)) ++ "." ++ s ++ " " ++ num_e,"pop temp 0"])
							}
							else fail ""
					})
			<|> 	try (do {c <- try (className)
					;	try (dot)
					;	s <- subroutineName
					;	e <- parens (expressionList thisClassName maps "push")
					;	let num_e = (snd e)
					;	let new_e = (fst e)
					;	return (unlines [new_e,"call " ++ c ++ "." ++ s ++ " " ++ num_e,"pop temp 0"])
					})
					
expressionList thisClassName maps cmd	= do{ l <- (commaSep (jackExpression thisClassName maps cmd)); return (unlines l, show (length l))}

className 		= try (do{ i <- identifier ; return i})
subroutineName	= try (do{ i <- identifier ; return i})
varName 		= try (do{ i <- identifier ; return i})

keywordConstant =	try (do{ try (reserved "true"); return "constant 0\nnot"})
				<|> try (do{ try (reserved "false"); return "constant 0"})
				<|> try (do{ try (reserved "null"); return "constant 0"})
				<|> try (do{ try (reserved "this"); return "pointer 0"})

statements thisClassName maps counters = do{ x <- many1 (try(statement thisClassName maps counters))
			;	 return (unlines x)
			}

statement thisClassName maps counters = letStmt thisClassName maps
		   <|> ifStmt thisClassName maps counters
           <|> whileStmt thisClassName maps counters
           <|> doStmt thisClassName maps
           <|> returnStmt thisClassName maps

letStmt thisClassName maps =	try (do {try (reserved "let")
			;	v <- try (varName)
			;	e1 <- try (squares (jackExpression thisClassName maps "push"))
			;	reservedOp "="
			;	e2 <- jackExpression thisClassName maps "push"
			;	semi
			;	return (unlines ["push " ++ vmVar maps v,e1,"add",e2,"pop pointer 1","push temp 0","pop that 0"])
				})
		<|>	do {reserved "let"
			;	v <- varName
			;	reservedOp "="
			;	e <- jackExpression thisClassName maps "push"
			;	semi
			;	return (unlines [e,"pop " ++ vmVar maps v])
				}

ifStmt thisClassName maps (i,w) =  try(do{try(reserved "if")
			;	e <- parens (try(jackExpression thisClassName maps "push"))
			;	s1 <- braces (try(statements thisClassName maps ((i+1),w)))
			;	reserved "else"
			;	s2 <- braces (statements thisClassName maps ((i+1),w))
			;	return (unlines[e,"if-goto IF_TRUE" ++ show i,"goto IF_FALSE" ++ show i,"label IF_TRUE" ++ show i,s1,"goto IF_END" ++ show i,"label IF_FALSE" ++ show i,s2,"label IF_END" ++ show i])
			})
		<|>	try(do{	try(reserved "if")
			;	e <- parens (try(jackExpression thisClassName maps "push"))
			;	s <- braces (try(statements thisClassName maps ((i+1),w)))
			;	return (unlines[e,"if-goto IF_TRUE" ++ show i,"goto IF_FALSE" ++ show i,"label IF_TRUE" ++ show i,s,"label IF_FALSE" ++ show i])
			})
			
whileStmt thisClassName maps (i,w) =	do{	reserved "while"
			;	e <- parens (jackExpression thisClassName maps "push")
			;	s <- braces (statements thisClassName maps (i,(w+1)))
			;	return (unlines["label WHILE_EXP" ++ show w,e,"not","if-goto WHILE_END" ++ show w,s,"goto WHILE_EXP" ++ show w,"label WHILE_END" ++ show w])
			}

doStmt thisClassName maps = 	do{	reserved "do"
			;	s <- subroutineCall thisClassName maps
			;	semi
			;	return s
			}

returnStmt thisClassName maps = try(	do{	reserved "return"
				;	e <- jackExpression thisClassName maps "push"
				;	semi
				;	return (unlines[e,"return"])
				})
			<|> do{	reserved "return"
				;	semi
				;	return (unlines["push constant 0","return"])
				}


stripExtraPops [] = []
stripExtraPops ('p':'o':'p':' ':'t':'e':'m':'p':' ':'0':'\n':'p':'o':'p':xs) = stripExtraPops ('p':'o':'p':xs)
stripExtraPops (x:xs) = x : stripExtraPops xs
				
stripExtraLineBr :: String -> String
stripExtraLineBr [] = []
stripExtraLineBr ('\n':'\n':xs) = stripExtraLineBr ('\n':xs)
stripExtraLineBr (x:xs) = x : stripExtraLineBr xs
				
parseString :: String -> IO String
parseString str = do 	{	case parse whileParser  "" str of
								Left e  -> error $ show e
								Right r -> return (stripExtraPops(stripExtraLineBr r))
						}

parseFile :: String -> IO String
parseFile file = do {	program  <- readFile file
					;   case parse whileParser "" program of
						   Left e  -> print e >> fail "parse error"
						   Right r -> return (stripExtraPops(stripExtraLineBr r))
					}