module Parser where

import           Text.Parsec.String             ( Parser )
import qualified Text.Parsec.Expr              as Ex
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token
import           Text.Parsec.Char               ( oneOf
                                                , letter
                                                , alphaNum
                                                )
import           Text.Parsec.Char
import           Control.Applicative            ( many
                                                , (<*)
                                                , (<$>)
                                                , (*>)
                                                , (<|>)
                                                , (<$)
                                                , (<*>)
                                                )
import           Text.Parsec                    ( try
                                                , parse
                                                , ParseError
                                                , (<?>)
                                                )
import           Text.Parsec.Combinator

import           Types
import           Print

langDef :: Token.LanguageDef ()
langDef = Token.LanguageDef
  { Token.commentStart    = "{-"
  , Token.commentEnd      = "}-"
  , Token.commentLine     = "--"
  , Token.nestedComments  = True
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum <|> oneOf "_"
  , Token.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.reservedNames = ["data", "let", "letrec", "case", "in", "of", "Pack"]
  , Token.reservedOpNames = [ "|"
                            , "\\"
                            , "->"
                            , "+"
                            , "-"
                            , "*"
                            , "/"
                            , "=="
                            , "<"
                            , ">"
                            , ">="
                            , "<="
                            , "!="
                            , "&&"
                            , "||"
                            ]
  , Token.caseSensitive   = True
  }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser langDef

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

natural :: Parser Integer
natural = Token.natural lexer

semi :: Parser String
semi = Token.semi lexer

comma :: Parser String
comma = Token.comma lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

uppercased :: Parser String
uppercased = do
  whiteSpace
  first <- upper
  rest  <- many (alphaNum <|> char '_' <|> char '\'')
  whiteSpace
  return (first : rest)

-- Parser entry point
parseProgram :: String -> IO ()
parseProgram s = putStrLn $ case parse pProgram "core" s of
  Left  _       -> "wrong"
  Right program -> pprint program

-- Program -> SuperCombinators
pProgram :: Parser Program
pProgram = do
  scs <- sepEndBy1 pSuperCombinator semi
  eof
  return scs

-- SuperCombinator -> var [ args]* = Expr;
pSuperCombinator :: Parser ScDefn
pSuperCombinator = do
  name <- identifier
  args <- many identifier
  reservedOp "="
  expr <- pExpression
  return (name, args, expr)

-- Expression -> Let | Case | Lambda
pExpression :: Parser Expr
pExpression = pLet <|> pCase <|> pLambda <|> pAExpression

-- Let -> (let | letrec) Bindings in Expr
pLet :: Parser Expr
pLet = do
  isRec    <- False <$ reserved "let" <|> True <$ reserved "letrec"
  bindings <- pBinding `sepEndBy1` semi
  expr     <- reserved "in" *> pExpression
  return (ELet isRec bindings expr)

-- Binding -> var = Expr
pBinding :: Parser (String, Expr)
pBinding = do
  name <- identifier
  reservedOp "="
  expr <- pExpression
  return (name, expr)

-- Case -> case expr of [alternative]
pCase :: Parser Expr
pCase = do
  expr <- reserved "case" *> pExpression
  reserved "of"
  alters <- pAlternative `sepEndBy1` semi
  return (ECase expr alters)

-- Alter -> Constructor [args]* -> expr
pAlternative :: Parser Alter
pAlternative = do
  name <- uppercased
  args <- many identifier
  expr <- reservedOp "->" *> pExpression
  return (PCon name, args, expr)

-- Lambda -> \x y -> expr
pLambda :: Parser Expr
pLambda = do
  args <- reservedOp "\\" *> many1 identifier
  expr <- reservedOp "->" *> pExpression
  return (ELam args expr)

pAExpression :: Parser Expr
pAExpression = Ex.buildExpressionParser operators terms

operators =
  [ [Ex.Prefix (reservedOp "-" >> return (EAp (EVar "-")))]
  , [ Ex.Infix (reservedOp "*" >> return (binOp (EVar "*"))) Ex.AssocLeft
    , Ex.Infix (reservedOp "/" >> return (binOp (EVar "/"))) Ex.AssocLeft
    ]
  , [ Ex.Infix (reservedOp "+" >> return (binOp (EVar "+"))) Ex.AssocLeft
    , Ex.Infix (reservedOp "-" >> return (binOp (EVar "-"))) Ex.AssocLeft
    ]
  , [ Ex.Infix (reservedOp "==" >> return (binOp (EVar "=="))) Ex.AssocLeft
    , Ex.Infix (reservedOp "!=" >> return (binOp (EVar "!="))) Ex.AssocLeft
    , Ex.Infix (reservedOp ">" >> return (binOp (EVar ">"))) Ex.AssocLeft
    , Ex.Infix (reservedOp ">=" >> return (binOp (EVar ">="))) Ex.AssocLeft
    , Ex.Infix (reservedOp "<" >> return (binOp (EVar "<"))) Ex.AssocLeft
    , Ex.Infix (reservedOp "<" >> return (binOp (EVar "<="))) Ex.AssocLeft
    ]
  , [ Ex.Infix (reservedOp "&&" >> return (binOp (EVar "&&"))) Ex.AssocLeft
    , Ex.Infix (reservedOp "||" >> return (binOp (EVar "||"))) Ex.AssocLeft
    ]
  ]

binOp :: Expr -> Expr -> Expr -> Expr
binOp op left right = foldl1 EAp [op, left, right]

-- Application -> Atom [Atom]*
terms :: Parser Expr
terms = makeSpine <$> many1 pAtom where makeSpine = foldl1 EAp

-- Atom -> Constructor | var | num | ( Expr )
pAtom :: Parser Expr
pAtom =
  parens pExpression
    <|> (EVar <$> identifier)
    <|> (ENum <$> natural)
    <?> "constructor, identifier, number, or parenthesized expression"
