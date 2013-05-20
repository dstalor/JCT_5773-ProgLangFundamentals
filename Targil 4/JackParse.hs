module JackParse where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.List

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

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
squares	   = Token.squares    lexer -- parses square brackets
braces	   = Token.braces     lexer -- parses curly brackets
decimal	   = Token.decimal    lexer -- parses natural decimal numbers
commaSep   = Token.commaSep   lexer -- parses a comma seperated list
commaSep1  = Token.commaSep1  lexer -- parses a comma seperated list
stringLiteral = Token.stringLiteral  lexer -- parses a literal string
symbol	   = Token.symbol 	  lexer -- parses a symbol
dot 	   = Token.dot  	  lexer -- parses a dot 


whileParser :: Parser String
whileParser = whiteSpace >> jackClass

jackClass 	= do {	reserved "class"
				;	c <- className
				;	symbol "{"
				;	cvd <- many classVarDec
				;	sd <- many subroutineDec
				;	symbol "}"
				;	return (unlines["<class>","<keyword> class </keyword>",c,"<symbol> { </symbol>",unlines cvd,unlines sd,"<symbol> } </symbol>","</class>"])
				}

classVarDec	= 	do {reserved "static"
				;	t <- jackType
				;	v <- commaSep1 varName
				;	semi
				;	return (unlines ["<classVarDec>","<keyword> static </keyword>",t,unlines(intersperse "<symbol> , </symbol>" v),"<symbol> ; </symbol>","</classVarDec>"])
				}
			<|>	do {reserved "field"
				;	t <- jackType
				;	v <- commaSep1 varName
				;	semi
				;	return (unlines ["<classVarDec>","<keyword> field </keyword>",t,unlines(intersperse "<symbol> , </symbol>" v),"<symbol> ; </symbol>","</classVarDec>"])
				}

jackType = 	do {reserved "int"; return ("<keyword> int </keyword>")}
		<|>	do {reserved "char"; return ("<keyword> char </keyword>")}
		<|>	do {reserved "boolean"; return ("<keyword> boolean </keyword>")}
		<|>	do {c <- className; return c}
		
subroutineDec =	do{	reserved "constructor"
				;	t  <- subroutineDecType
				;	sn <- subroutineName
				;	pl <- parens parameterList
				;	sb <- subroutineBody
				;	return (unlines ["<subroutineDec>","<keyword> constructor </keyword>",t,sn,"<symbol> ( </symbol>",pl,"<symbol> ) </symbol>",sb,"</subroutineDec>"])
				}
			<|>	do{	reserved "function"
				;	t  <- subroutineDecType
				;	sn <- subroutineName
				;	pl <- parens parameterList
				;	sb <- subroutineBody
				;	return (unlines ["<subroutineDec>","<keyword> function </keyword>",t,sn,"<symbol> ( </symbol>",pl,"<symbol> ) </symbol>",sb,"</subroutineDec>"])
				}
			<|>	do{	reserved "method"
				;	t  <- subroutineDecType
				;	sn <- subroutineName
				;	pl <- parens parameterList
				;	sb <- subroutineBody
				;	return (unlines ["<subroutineDec>","<keyword> method </keyword>",t,sn,"<symbol> ( </symbol>",pl,"<symbol> ) </symbol>",sb,"</subroutineDec>"])
				}
				
subroutineDecType =	try (do {reserved "void"; return ("<keyword> void </keyword>")})
				<|>	do {t <- jackType; return t}
				
parameterList =	do{p <- commaSep param; return (unlines["<parameterList>",unlines(intersperse "<symbol> , </symbol>" p),"</parameterList>"])}
param = do {t <- jackType; v <- varName; return (unlines [t,v])}

subroutineBody = do{ symbol "{"
				;	 v <- many varDec
				;	 s <- statements
				;	 symbol "}"
				;	 return (unlines ["<subroutineBody>","<symbol> { </symbol>",unlines v,s,"<symbol> } </symbol>","</subroutineBody>"])
				}

varDec	= do {	reserved "var"
			;	p <- commaSep1 param
			;	semi
			;	return (unlines ["<varDec>","<keyword> var </keyword>",unlines(intersperse "<symbol> , </symbol>" p),"<symbol> ; </symbol>","</varDec>"])
			}
				
jackOp = try (do{ reservedOp "+"; return ("<symbol> + </symbol>")})
	<|> try (do{ reservedOp  "-"; return ("<symbol> - </symbol>")})
	<|> try (do{ reservedOp  "*"; return ("<symbol> * </symbol>")})
	<|> try (do{ reservedOp  "/"; return ("<symbol> / </symbol>")})
	<|> try (do{ reservedOp  "&"; return ("<symbol> &amp; </symbol>")})
	<|> try (do{ reservedOp  "|"; return ("<symbol> | </symbol>")})
	<|> try (do{ reservedOp  "<"; return ("<symbol> &lt; </symbol>")})
	<|> try (do{ reservedOp  ">"; return ("<symbol> &gt; </symbol>")})
	<|> try (do{ reservedOp  "="; return ("<symbol> = </symbol>")})

unaryOp = try (do{ reservedOp  "-"; return ("<symbol> - </symbol>")})
	<|> try (do{ reservedOp  "~"; return ("<symbol> ~ </symbol>")})
			
jackExpression = try (do{t <- try (jackTerm); return (unlines ["<expression>",t,"</expression>"])})
				<|> do {t1 <- jackTerm
					; o <- jackOp
					; t2 <- jackTerm
					; return (unlines ["<expression>",t1,o,t2,"</expression>"])
					}

integerConstant = try (do{i <- try (decimal); 	return ("<integerConstant> " ++ show i ++ " </integerConstant>")})

stringConstant = try (do{s <- try (stringLiteral); return ("<stringConstant> " ++ s ++ " </stringConstant>")})

jackTerm =  do{ t <- integerConstant; return (unlines ["<term>",t,"</term>"])}
		<|> do{ t <- stringConstant; return (unlines ["<term>",t,"</term>"])}
		<|> do{ t <- keywordConstant; return (unlines ["<term>",t,"</term>"])}
		<|> do{ t <- subroutineCall; return (unlines ["<term>",t,"</term>"])}
		<|> do{ t <- varName; return (unlines ["<term>",t,"</term>"])}
		<|> try (do{ v <- try (varName)
				;	 e <- try (squares jackExpression)
				;	 return (unlines ["<term>",v,"<symbol> [ </symbol>",e,"<symbol> ] </symbol>","</term>"])
				})
		<|> try (do{ e <- try (parens jackExpression); return (unlines ["<term>","<symbol> ( </symbol>",e,"<symbol> ) </symbol>","</term>"])})
		<|> try (do{ o <- try (unaryOp)
				;	 t <- try (jackTerm)
				; 	 return (unlines ["<term>",t,"</term>"])
				})

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
			<|> 	do {v <- varName
					;	dot
					;	s <- subroutineName
					;	e <- parens expressionList
					;	return (unlines [v,"<symbol> . </symbol>",s,"<symbol> ( </symbol>",e,"<symbol> ) </symbol>"])
					}
					
expressionList 	= do{ l <- commaSep jackExpression; return (unlines ["<expressionList>",unlines l,"</expressionList>"])}

className 		= do{ i <- identifier ; return ("<identifier> " ++ i ++ " </identifier>")}
subroutineName	= do{ i <- identifier ; return ("<identifier> " ++ i ++ " </identifier>")}
varName 		= do{ i <- identifier ; return ("<identifier> " ++ i ++ " </identifier>")}

keywordConstant =	try (do{ try (reserved "true"); return "<keyword> true </keyword>"})
				<|> try (do{ try (reserved "false"); return "<keyword> false </keyword>"})
				<|> try (do{ try (reserved "null"); return "<keyword> null </keyword>"})
				<|> try (do{ try (reserved "this"); return "<keyword> this </keyword>"})

statements :: Parser String
statements = do{ x <- many1 statement
			;	 return (unlines ["<statements>",unlines x,"</statements>"])
			}

statement :: Parser String
statement = letStmt
		   <|> ifStmt
           <|> whileStmt
           <|> doStmt
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
ifStmt =  	do{	reserved "if"
			;	e <- parens jackExpression
			;	s1 <- braces statements
			;	reserved "else"
			;	s2 <- braces statements
			;	return (unlines["<ifStatement>","<keyword> if </keyword>","<symbol> ( </symbol>",e,"<symbol> ) </symbol>","<symbol> { </symbol>",s1,"<symbol> } </symbol>","<keyword> else </keyword>","<symbol> { </symbol>",s2,"<symbol> } </symbol>","</ifStatement>"])
			}
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

parseString :: String -> IO ()
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> putStr r
 
parseFile :: String -> IO ()
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> putStr r