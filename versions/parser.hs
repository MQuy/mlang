{-# LANGUAGE TupleSections #-}

import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, digit, string, anyChar, char, letter, alphaNum)
import Text.Parsec.Combinator (many1, manyTill, eof, choice, between
                                     ,sepBy, optionMaybe, sepBy1, manyTill, anyToken)
import Text.Parsec (try, parse, ParseError)
import Control.Applicative (many, (<*),(<$>), (*>), (<|>),(<$),(<*>))
import Control.Monad (liftM, void,guard)
import qualified Text.Parsec.Expr as Ex
import qualified Text.ParserCombinators.Parsec.Token as Token

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
  , Token.reservedOpNames = ["+", "-", "*", "/", "="
                             , "<", ">", "and", "or", "not"
                             ]
  , Token.caseSensitive   = True
  }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser langDef

identifier = Token.identifier       lexer -- parses an identifier
reserved   = Token.reserved         lexer -- parses a reserved name
reservedOp = Token.reservedOp       lexer -- parses an operator
parens     = Token.parens           lexer -- parses surrounding parenthesis:
integer    = Token.integer          lexer -- parses an integer
semi       = Token.semi             lexer -- parses a semicolon
whiteSpace = Token.whiteSpace       lexer -- parses whitespace
stringLiteral = Token.stringLiteral lexer -- parses a string

statement :: Parser Statement
statement =
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
  condition <- expression
  thenStatement <- statement
  reserved "else"
  elseStatement <- statement
  return $ If condition thenStatement elseStatement

blockStatement :: Parser Statement
blockStatement = do
  char '{'
  list <- (sepBy statement semi)
  char '}'
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
  char '='
  expr <- expression
  semi
  return $ VarStatement name expr

whileStatement :: Parser Statement
whileStatement = do
  reserved "while"
  condition <- expression
  stmt <- statement
  return $ While condition stmt

expressionStatement :: Parser Statement
expressionStatement = do
  expr <- expression
  semi
  return $ ExpressionStatement expr

expression :: Parser Expression
expression = Ex.buildExpressionParser operators terms

operators = [
              [Ex.Prefix (reservedOp "-" >> return Neg)],
              [
                Ex.Infix (reservedOp "*" >> return (Binary Multiply)) Ex.AssocLeft,
                Ex.Infix (reservedOp "/" >> return (Binary Divide)) Ex.AssocLeft
              ],
              [
                Ex.Infix (reservedOp "+" >> return (Binary Add)) Ex.AssocLeft,
                Ex.Infix (reservedOp "-" >> return (Binary Subtract)) Ex.AssocLeft
              ],
              [Ex.Prefix (reservedOp "not" >> return Not)],
              [
                Ex.Infix (reservedOp "and" >> return (Binary And)) Ex.AssocLeft,
                Ex.Infix (reservedOp "or" >> return (Binary Or)) Ex.AssocLeft
              ]
            ]

terms =
  parens expression
  <|> Var <$> identifier
  <|> IntConst <$> integer
  <|> StringConst <$> stringLiteral
  <|> (reserved "true" >> return (BoolConst True))
  <|> (reserved "false" >> return (BoolConst False))