module SimpleParser where

import           Data.Char
import           Control.Monad
import           Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a, String)]}

item :: Parser Char
item = Parser $ \s -> case s of
  []       -> []
  (c : cs) -> [(c, cs)]

instance Functor Parser where
  fmap f (Parser cs) = Parser $ \s -> [(f a, s1) | (a, s1) <- cs s]

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser $ \s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1]

instance Monad Parser where
  return a = Parser $ \s -> [(a, s)]
  p >>= f = Parser $ \s -> concat [parse (f a) s1 | (a, s1) <- parse p s]

instance Alternative Parser where
  empty = failure
  (<|>) = option

failure :: Parser a
failure = Parser $ \_ -> []

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  c <- item
  case f c of
    True  -> return c
    False -> failure

char :: Char -> Parser Char
char c = satisfy (c ==)

string :: String -> Parser String
string []       = return []
string (c : cs) = do
  char c
  string cs
  return (c : cs)

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s -> case parse p s of
  []  -> parse q s
  res -> res

many1 :: Parser a -> Parser [a]
many1 p = do
  c  <- p
  cs <- many p
  return (c : cs)

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p q = Parser $ \s -> case parse p s of
  []        -> []
  [(c, cs)] -> case parse q cs of
    []          -> []
    [(c1, cs1)] -> case parse p cs1 of
      []  -> [([c], cs1)]
      res -> [foldl sepBy1Combine ([c], cs1) ((parse (sepBy1 p q) cs1))]

sepBy1Combine :: ([a], String) -> ([a], String) -> ([a], String)
sepBy1Combine (c1, _) (c2, s2) = (c1 ++ c2, s2)
