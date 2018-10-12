{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaCalculus where

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
import           Text.Parsec.Language           ( haskellStyle )
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
import           Data.Functor.Identity
import qualified Data.Text.Lazy                as Lazy

-- Parser
type Name = String

data Expr =
  Var Name
  | App Expr Expr
  | Lam Name Expr
  | Let Name Expr Expr
  | Lit Lit
  | If Expr Expr Expr
  | Op BinOp Expr Expr
  deriving (Eq, Show)

data Lit =
  LInt Integer
  | LBool Bool
  deriving (Eq, Show)

data BinOp = Add | Sub | Mul | Eql deriving (Eq, Ord, Show)

langDef :: Token.LanguageDef ()
langDef = haskellStyle
  { Token.commentStart    = "{-"
  , Token.commentEnd      = "-}"
  , Token.commentLine     = "--"
  , Token.nestedComments  = True
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum <|> oneOf "_"
  , Token.reservedOpNames = ["\\", "->", ".", "+", "-", "*", "/", "="]
  , Token.reservedNames   = ["let", "in", "if", "then", "else"]
  }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser langDef

identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

semiSep :: Parser a -> Parser [a]
semiSep = Token.semiSep lexer

semi :: Parser String
semi = Token.semi lexer

integer :: Parser Integer
integer = Token.integer lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)

number :: Parser Expr
number = do
  n <- integer
  return (Lit (LInt (fromIntegral n)))

bool :: Parser Expr
bool =
  (reserved "True" >> return (Lit (LBool True)))
    <|> (reserved "False" >> return (Lit (LBool False)))

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  name <- identifier
  reservedOp "->"
  body <- expr
  return $ Lam name body

letin :: Parser Expr
letin = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return (Let x e1 e2)

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return (If cond tr fl)

aexp :: Parser Expr
aexp =
  parens expr
    <|> ifthen
    <|> letin
    <|> lambda
    <|> variable
    <|> bool
    <|> number

term =
  aexp

table =
  [ [Ex.Infix (reservedOp "*" >> return (Op Mul)) Ex.AssocLeft]
  , [ Ex.Infix (reservedOp "+" >> return (Op Add)) Ex.AssocLeft
    , Ex.Infix (reservedOp "-" >> return (Op Sub)) Ex.AssocLeft
    ]
  , [Ex.Infix (reservedOp "==" >> return (Op Eql)) Ex.AssocLeft]
  ]

expr :: Parser Expr
expr = Ex.buildExpressionParser table term

program :: Parser [Expr]
program = do
  exprs <- endBy1 expr semi
  eof
  return exprs

parseExpr :: String -> Either ParseError [Expr]
parseExpr input = parse program "<stdin>" input

-- Types
newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TCon String
  | TArr Type Type
  deriving (Show, Eq, Ord)

data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)

typeInt, typeBool :: Type
typeInt  = TCon "Int"
typeBool = TCon "Bool"

-- Infer
