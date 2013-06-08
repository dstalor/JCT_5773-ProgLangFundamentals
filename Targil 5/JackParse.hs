module JackParse where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.List as List
import qualified Data.Map as Map

mapIndexer m s 	= (Map.fromList (List.map (\(x,y) -> (x,(y,s,(Map.findIndex x m)))) (Map.toList m)))
showMap m 		= (Map.showTreeWith (\k (x,y,z) -> (show k) ++ "|" ++ (show x) ++ "|" ++ (show y) ++ "|" ++ (show z)) False False m)

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

identifier 		= Token.identifier lexer -- parses an identifier
reserved   		= Token.reserved   lexer -- parses a reserved name
reservedOp 		= Token.reservedOp lexer -- parses an operator
parens     		= Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    		= Token.integer    lexer -- parses an integer
semi       		= Token.semi       lexer -- parses a semicolon
whiteSpace 		= Token.whiteSpace lexer -- parses whitespace
squares	   		= Token.squares    lexer -- parses square brackets
braces	   		= Token.braces     lexer -- parses curly brackets
decimal	   		= Token.decimal    lexer -- parses natural decimal numbers
commaSep   		= Token.commaSep   lexer -- parses a comma seperated list
commaSep1  		= Token.commaSep1  lexer -- parses a comma seperated list
stringLiteral 	= Token.stringLiteral  	lexer -- parses a literal string
symbol	   		= Token.symbol 	  		lexer -- parses a symbol
dot 	   		= Token.dot  	  		lexer -- parses a dot 
lexeme 			= Token.lexeme			lexer

whileParser :: Parser String
whileParser = whiteSpace >> jackClass

jackClass 	= do {	reserved "class"
				;	c <- className
				;	let staticMap = Map.empty
				;	let fieldMap = Map.empty
				;	symbol "{"
				;	cvd <- many (classVarDec staticMap fieldMap)
				;	let staticMap = (mapIndexer (Map.unions (List.map (\(x,(y,z)) -> y) cvd)) "static")
				;	let fieldMap = (mapIndexer (Map.unions (List.map (\(x,(y,z)) -> z) cvd)) "field")
				;	let classMap = (Map.union staticMap fieldMap)
				;	let cvd2 = (List.map (\(x,(y,z)) -> x) cvd)
				;	sd <- many subroutineDec
				;	symbol "}"
				;	return (unlines["<class>",showMap classMap,"<keyword> class </keyword>",c,"<symbol> { </symbol>",unlines cvd2,unlines sd,"<symbol> } </symbol>","</class>"])
				}

classVarDec	staticMap fieldMap = 	do {reserved "static"
				;	t <- jackType
				;	v <- commaSep1 varName
				;	semi
				;	let newStaticMap = (Map.union (Map.fromList (zip v (repeat t))) staticMap)
				;	return ((unlines ["<classVarDec>","<keyword> static </keyword>",t,unlines(List.intersperse "<symbol> , </symbol>" v),"<symbol> ; </symbol>","</classVarDec>"]),(newStaticMap,fieldMap))
				}
			<|>	do {reserved "field"
				;	t <- jackType
				;	v <- commaSep1 varName
				;	semi
				;	let newFieldMap = (Map.union (Map.fromList (zip v (repeat t))) fieldMap)
				;	return ((unlines ["<classVarDec>","<keyword> field </keyword>",t,unlines (List.intersperse "<symbol> , </symbol>" v),"<symbol> ; </symbol>","</classVarDec>"]),(staticMap,newFieldMap))
				}

jackType = 	do {reserved "int"; return ("<keyword> int </keyword>")}
		<|>	do {reserved "char"; return ("<keyword> char </keyword>")}
		<|>	do {reserved "boolean"; return ("<keyword> boolean </keyword>")}
		<|>	do {c <- className; return c}
		
subroutineDec =	do{	reserved "constructor"
				;	let argMap = Map.empty
				;	let varMap = Map.empty
				;	t  <- subroutineDecType
				;	sn <- subroutineName
				;	pl <- parens (parameterList argMap)
				;	let argMap = (mapIndexer (snd pl) "argument")
				;	let pl2 = (fst pl)
				;	sb <- subroutineBody varMap
				;	let varMap = (mapIndexer (snd sb) "var")
				;	let sb2 = (fst sb)
				;	let methodMap = (Map.union argMap varMap)
				;	return (unlines ["<subroutineDec>",showMap methodMap,"<keyword> constructor </keyword>",t,sn,"<symbol> ( </symbol>",pl2,"<symbol> ) </symbol>",sb2,"</subroutineDec>"])
				}
			<|>	do{	reserved "function"
				;	let argMap = Map.empty
				;	let varMap = Map.empty
				;	t  <- subroutineDecType
				;	sn <- subroutineName
				;	pl <- parens (parameterList argMap)
				;	let argMap = (mapIndexer (snd pl) "argument")
				;	let pl2 = (fst pl)
				;	sb <- subroutineBody varMap
				;	let varMap = (mapIndexer (snd sb) "var")
				;	let sb2 = (fst sb)
				;	let methodMap = (Map.union argMap varMap)
				;	return (unlines ["<subroutineDec>",showMap methodMap,"<keyword> function </keyword>",t,sn,"<symbol> ( </symbol>",pl2,"<symbol> ) </symbol>",sb2,"</subroutineDec>"])
				}
			<|>	do{	reserved "method"
				;	let argMap = Map.empty
				;	let varMap = Map.empty
				;	t  <- subroutineDecType
				;	sn <- subroutineName
				;	pl <- parens (parameterList argMap)
				;	let argMap = (mapIndexer (snd pl) "argument")
				;	let pl2 = (fst pl)
				;	sb <- subroutineBody varMap
				;	let varMap = (mapIndexer (snd sb) "var")
				;	let sb2 = (fst sb)
				;	let methodMap = (Map.union argMap varMap)
				;	return (unlines ["<subroutineDec>",showMap methodMap,"<keyword> method </keyword>",t,sn,"<symbol> ( </symbol>",pl2,"<symbol> ) </symbol>",sb2,"</subroutineDec>"])
				}
				
subroutineDecType =	try (do {reserved "void"; return ("<keyword> void </keyword>")})
				<|>	do {t <- jackType; return t}
				
parameterList argMap 	= do	{	p <- commaSep (param argMap)
								;	let p2 = (List.map (\(x,y) -> x) p)
								;	let argMap = (Map.unions (List.map (\(x,y) -> y) p))
								; 	return ((unlines["<parameterList>",unlines(List.intersperse "<symbol> , </symbol>" p2),"</parameterList>"]),argMap)
								}
									
param argMap 			= do 	{	t <- jackType
								; 	v <- varName
								;	let newArgMap = (Map.singleton v t)
								; 	return ((unlines [t,v]),newArgMap)
								}

subroutineBody varMap 	= do	{	symbol "{"
								;	v <- many (varDec varMap)
								;	let v2 = (List.map (\(x,y) -> x) v)
								;	let varMap = (Map.unions (List.map (\(x,y) -> y) v))
								;	s <- statements
								;	symbol "}"
								;	return ((unlines ["<subroutineBody>","<symbol> { </symbol>",unlines v2,s,"<symbol> } </symbol>","</subroutineBody>"]),varMap)
								}

varDec varMap			= do 	{	reserved "var"
								;	t <- jackType
								;	v <- commaSep1 varName
								;	let newVarMap = (Map.union (Map.fromList (zip v (repeat t))) varMap)
								;	semi
								;	return ((unlines ["<varDec>","<keyword> var </keyword>",t,unlines(List.intersperse "<symbol> , </symbol>" v),"<symbol> ; </symbol>","</varDec>"]),newVarMap)
								}
				
jackOp = do{ reservedOp "+"; return ("<symbol> + </symbol>")}
	<|> do{ reservedOp  "-"; return ("<symbol> - </symbol>")}
	<|> do{ reservedOp  "*"; return ("<symbol> * </symbol>")}
	<|> do{ reservedOp  "/"; return ("<symbol> / </symbol>")}
	<|> do{ reservedOp  "&"; return ("<symbol> &amp; </symbol>")}
	<|> do{ reservedOp  "|"; return ("<symbol> | </symbol>")}
	<|> do{ reservedOp  "<"; return ("<symbol> &lt; </symbol>")}
	<|> do{ reservedOp  ">"; return ("<symbol> &gt; </symbol>")}
	<|> do{ reservedOp  "="; return ("<symbol> = </symbol>")}

unaryOp = do{ reservedOp  "-"; return ("<symbol> - </symbol>")}
	<|> do{ reservedOp  "~"; return ("<symbol> ~ </symbol>")}
			
jackExpression = try (do	{	t1 <- jackTerm
						; 	o  <- jackOp
						; 	t2 <- jackTerm
						; 	return (unlines ["<expression>",t1,o,t2,"</expression>"])
						})
				<|> try (do{t  <- try (jackTerm); return (unlines ["<expression>",t,"</expression>"])})

integerConstant = try (do{i <- try (decimal); 	return ("<integerConstant> " ++ show i ++ " </integerConstant>")})

stringConstant = try (do{s <- try (stringLiteral); return ("<stringConstant> " ++ s ++ " </stringConstant>")})

jackTerm =  try (do	{ 	t <- integerConstant; return (unlines ["<term>",t,"</term>"])})
		<|> try (do	{ 	t <- stringConstant; return (unlines ["<term>",t,"</term>"])})
		<|> try (do	{ 	t <- try (keywordConstant); return (unlines ["<term>",t,"</term>"])})
		<|> try (do	{ 	t <- try (subroutineCall); return (unlines ["<term>",t,"</term>"])})
		<|> try (do	{ 	o <- try (unaryOp)
					;	t <- try (jackTerm)
					; 	return (unlines ["<term>",o,t,"</term>"])
					})
		<|> try (do{ e <- try (parens jackExpression); return (unlines ["<term>","<symbol> ( </symbol>",e,"<symbol> ) </symbol>","</term>"])})
		<|> try (do{ v <- try (varName)
				;	 e <- try (squares jackExpression)
				;	 return (unlines ["<term>",v,"<symbol> [ </symbol>",e,"<symbol> ] </symbol>","</term>"])
				})
		<|> try (do{ t <- try(varName); return (unlines ["<term>",t,"</term>"])})

subroutineCall = 	try (do {s <- try (subroutineName)
					;	e <- try (parens expressionList)
					;	return (unlines [s,"<symbol> ( </symbol>",e,"<symbol> ) </symbol>"])
					})
			<|>		try (do {c <- try (className)
					;	try (dot)
					;	s <- try (subroutineName)
					;	e <- try (parens expressionList)
					;	return (unlines [c,"<symbol> . </symbol>",s,"<symbol> ( </symbol>",e,"<symbol> ) </symbol>"])
					})
			<|> 	try (do {v <- try (varName)
					;	try (dot)
					;	s <- try (subroutineName)
					;	e <- try (parens expressionList)
					;	return (unlines [v,"<symbol> . </symbol>",s,"<symbol> ( </symbol>",e,"<symbol> ) </symbol>"])
					})
					
expressionList 	= do{ l <- (commaSep (lexeme jackExpression)); return (unlines ["<expressionList>",unlines(List.intersperse "<symbol> , </symbol>" l),"</expressionList>"])}

className 		= try (do{ i <- identifier ; return ("<identifier> " ++ i ++ " </identifier>")})
subroutineName	= try (do{ i <- identifier ; return ("<identifier> " ++ i ++ " </identifier>")})
varName 		= try (do{ i <- identifier ; return ("<identifier> " ++ i ++ " </identifier>")})

keywordConstant =	try (do{ try (reserved "true"); return "<keyword> true </keyword>"})
				<|> try (do{ try (reserved "false"); return "<keyword> false </keyword>"})
				<|> try (do{ try (reserved "null"); return "<keyword> null </keyword>"})
				<|> try (do{ try (reserved "this"); return "<keyword> this </keyword>"})

statements :: Parser String
statements = do{ x <- many1 statement
			;	 return (unlines ["<statements>",unlines x,"</statements>"])
			}

statement :: Parser String
statement = try (letStmt)
		   <|> try (ifStmt)
           <|> try (whileStmt)
           <|> try (doStmt)
           <|> returnStmt

letStmt :: Parser String
letStmt =	try (do {try (reserved "let")
			;	v <- try (varName)
			;	e1 <- try (squares jackExpression)
			;	reservedOp "="
			;	e2 <- jackExpression
			;	semi
			;	return (unlines ["<letStatement>","<keyword> let </keyword>",v,"<symbol> [ </symbol>",e1,"<symbol> ] </symbol>","<symbol> = </symbol>",e2,"<symbol> ; </symbol>","</letStatement>"])
				})
		<|>	do {reserved "let"
			;	v <- varName
			;	reservedOp "="
			;	e <- jackExpression
			;	semi
			;	return (unlines ["<letStatement>","<keyword> let </keyword>",v,"<symbol> = </symbol>",e,"<symbol> ; </symbol>","</letStatement>"])
				}

ifStmt :: Parser String
ifStmt =  try(	do{	reserved "if"
			;	e <- parens jackExpression
			;	s1 <- braces statements
			;	reserved "else"
			;	s2 <- braces statements
			;	return (unlines["<ifStatement>","<keyword> if </keyword>","<symbol> ( </symbol>",e,"<symbol> ) </symbol>","<symbol> { </symbol>",s1,"<symbol> } </symbol>","<keyword> else </keyword>","<symbol> { </symbol>",s2,"<symbol> } </symbol>","</ifStatement>"])
			})
		<|>	do{	reserved "if"
			;	e <- parens jackExpression
			;	s <- braces statements
			;	return (unlines["<ifStatement>","<keyword> if </keyword>","<symbol> ( </symbol>",e,"<symbol> ) </symbol>","<symbol> { </symbol>",s,"<symbol> } </symbol>","</ifStatement>"])
			}
			
whileStmt :: Parser String
whileStmt =	do{	reserved "while"
			;	e <- parens jackExpression
			;	s <- braces statements
			;	return (unlines["<whileStatement>","<keyword> while </keyword>","<symbol> ( </symbol>",e,"<symbol> ) </symbol>","<symbol> { </symbol>",s,"<symbol> } </symbol>","</whileStatement>"])
			}

doStmt :: Parser String
doStmt = 	do{	reserved "do"
			;	s <- subroutineCall
			;	semi
			;	return (unlines["<doStatement>","<keyword> do </keyword>",s,"<symbol> ; </symbol>","</doStatement>"])
			}

returnStmt :: Parser String
returnStmt = try(	do{	reserved "return"
				;	e <- jackExpression
				;	semi
				;	return (unlines["<returnStatement>","<keyword> return </keyword>",e,"<symbol> ; </symbol>","</returnStatement>"])
				})
			<|> do{	reserved "return"
				;	semi
				;	return (unlines["<returnStatement>","<keyword> return </keyword>","<symbol> ; </symbol>","</returnStatement>"])
				}

				
stripExtraLineBr :: String -> String
stripExtraLineBr [] = []
stripExtraLineBr ('\n':'\n':xs) = stripExtraLineBr ('\n':xs)
stripExtraLineBr (x:xs) = x : stripExtraLineBr xs
				
parseString :: String -> IO String
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> return (stripExtraLineBr r)
 
parseFile :: String -> IO String
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return (stripExtraLineBr r)