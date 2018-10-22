module Lang where

import           Text.Parsec.String             ( Parser )
import           Text.Parsec.Char               ( oneOf
                                                , digit
                                                , string
                                                , anyChar
                                                , char
                                                , letter
                                                , alphaNum
                                                )
import           Text.Parsec.Combinator         ( many1
                                                , manyTill
                                                , eof
                                                , choice
                                                , between
                                                , sepBy
                                                , optionMaybe
                                                , sepBy1
                                                , manyTill
                                                , anyToken
                                                )
import           Text.Parsec.Combinator
import           Text.Parsec                    ( try
                                                , parse
                                                , ParseError
                                                )
import           Control.Applicative            ( many
                                                , (<*)
                                                , (<$>)
                                                , (*>)
                                                , (<|>)
                                                , (<$)
                                                , (<*>)
                                                )
import           Control.Monad                  ( liftM
                                                , void
                                                , guard
                                                )
import qualified Text.Parsec.Expr              as Ex
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token

data Statement =
  If Expression Statement Statement
  | Block [Statement]
  | Break
  | Continue
  | VarStatement String Expression
  | While Expression Statement
  | ExpressionStatement Expression
  deriving Show

data Expression =
  BoolConst Bool
  | IntConst Integer
  | StringConst String
  | Var String
  | Neg Expression
  | Not Expression
  | Binary BinOp Expression Expression
  deriving Show

data BinOp =
  Add
  | Subtract
  | Multiply
  | Divide
  | And
  | Or
  deriving Show

langDef :: Token.LanguageDef ()
langDef = Token.LanguageDef
  { Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.commentLine     = "//"
  , Token.nestedComments  = True
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum <|> oneOf "_"
  , Token.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.reservedNames   = [ "if"
                            , "then"
                            , "else"
                            , "while"
                            , "do"
                            , "skip"
                            , "true"
                            , "false"
                            , "not"
                            , "and"
                            , "or"
                            , "break"
                            , "continue"
                            , "var"
                            ]
  , Token.reservedOpNames = [ "+"
                            , "-"
                            , "*"
                            , "/"
                            , "="
                            , "<"
                            , ">"
                            , "and"
                            , "or"
                            , "not"
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

integer :: Parser Integer
integer = Token.integer lexer

semi :: Parser String
semi = Token.semi lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

program :: Parser [Statement]
program = do
  statements <- endBy1 statement semi
  eof
  return statements

statement :: Parser Statement
statement = whiteSpace >> statementWithoutSpace <* whiteSpace

statementWithoutSpace :: Parser Statement
statementWithoutSpace =
  ifStatement
    <|> blockStatement
    <|> breakStatement
    <|> continueStatement
    <|> varStatement
    <|> whileStatement
    <|> expressionStatement

ifStatement :: Parser Statement
ifStatement = do
  reserved "if"
  condition     <- expression
  thenStatement <- statement
  reserved "else"
  elseStatement <- statement
  return $ If condition thenStatement elseStatement

blockStatement :: Parser Statement
blockStatement = do
  symbol "{"
  list <- (sepBy statement semi)
  symbol "}"
  return $ Block list

breakStatement :: Parser Statement
breakStatement = do
  reserved "break"
  semi
  return Break

continueStatement :: Parser Statement
continueStatement = do
  reserved "continue"
  semi
  return Continue

varStatement :: Parser Statement
varStatement = do
  reserved "var"
  name <- identifier
  symbol "="
  expr <- expression
  semi
  return $ VarStatement name expr

whileStatement :: Parser Statement
whileStatement = do
  reserved "while"
  condition <- expression
  stmt      <- statement
  return $ While condition stmt

expressionStatement :: Parser Statement
expressionStatement = do
  expr <- expression
  semi
  return $ ExpressionStatement expr

expression :: Parser Expression
expression = Ex.buildExpressionParser operators terms

operators =
  [ [Ex.Prefix (reservedOp "-" >> return Neg)]
  , [ Ex.Infix (reservedOp "*" >> return (Binary Multiply)) Ex.AssocLeft
    , Ex.Infix (reservedOp "/" >> return (Binary Divide)) Ex.AssocLeft
    ]
  , [ Ex.Infix (reservedOp "+" >> return (Binary Add)) Ex.AssocLeft
    , Ex.Infix (reservedOp "-" >> return (Binary Subtract)) Ex.AssocLeft
    ]
  , [Ex.Prefix (reservedOp "not" >> return Not)]
  , [ Ex.Infix (reservedOp "and" >> return (Binary And)) Ex.AssocLeft
    , Ex.Infix (reservedOp "or" >> return (Binary Or)) Ex.AssocLeft
    ]
  ]


terms :: Parser Expression
terms =
  parens expression
    <|> (Var <$> identifier)
    <|> (IntConst <$> integer)
    <|> (StringConst <$> stringLiteral)
    <|> (reserved "true" >> return (BoolConst True))
    <|> (reserved "false" >> return (BoolConst False))
