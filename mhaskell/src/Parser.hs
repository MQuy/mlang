module Parser
  ( parse
  )
where
import qualified Text.Parsec.Expr              as Ex
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token
import qualified Data.Map                      as M
import           Text.Parsec.Char               ( letter
                                                , alphaNum
                                                , oneOf
                                                , char
                                                , upper
                                                , lower
                                                )
import           Control.Applicative            ( many
                                                , (<*)
                                                , (<$>)
                                                , (*>)
                                                , (<|>)
                                                , (<$)
                                                , (<*>)
                                                )
import           Text.Parsec                    ( try
                                                , runP
                                                , ParseError
                                                , (<?>)
                                                )
import           Text.Parsec.Combinator         ( sepEndBy
                                                , sepEndBy1
                                                , many1
                                                , eof
                                                )
import           Text.Parsec.Prim               ( ParsecT
                                                , modifyState
                                                , getState
                                                , unexpected
                                                , Parsec
                                                )
import           Data.Char                      ( chr )
import           Type

type SParser = Parsec String PEInfo

reservedNames = ["data", "let", "letrec", "case", "in", "of"]
reservedOpNames =
  [ "|"
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
  , "{"
  , "}"
  ]

langDef :: Token.LanguageDef PEInfo
langDef = Token.LanguageDef
  { Token.commentStart    = "{-"
  , Token.commentEnd      = "-}"
  , Token.commentLine     = "--"
  , Token.nestedComments  = True
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum <|> oneOf "_"
  , Token.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.reservedNames   = reservedNames
  , Token.reservedOpNames = reservedOpNames
  , Token.caseSensitive   = True
  }

lexer :: Token.TokenParser PEInfo
lexer = Token.makeTokenParser langDef

reserved :: String -> SParser ()
reserved = Token.reserved lexer

reservedOp :: String -> SParser ()
reservedOp = Token.reservedOp lexer

parens :: SParser a -> SParser a
parens = Token.parens lexer

natural :: SParser Integer
natural = Token.natural lexer

semi :: SParser String
semi = Token.semi lexer

whiteSpace :: SParser ()
whiteSpace = Token.whiteSpace lexer

symbol :: String -> SParser String
symbol = Token.symbol lexer

pUppercased :: SParser String
pUppercased = try $ do
  first <- whiteSpace *> upper
  rest  <- many (alphaNum <|> char '_' <|> char '\'') <* whiteSpace
  nameParser $ first : rest

pVariable :: SParser String
pVariable = try $ do
  first <- whiteSpace *> lower
  rest  <- many (alphaNum <|> char '_' <|> char '\'') <* whiteSpace
  nameParser $ first : rest

nameParser :: String -> SParser String
nameParser name = if name `elem` reservedNames
  then unexpected ("reserved word " ++ show name)
  else return name

pProgram :: SParser Program
pProgram = do
  defs <- sepEndBy (pScData <|> pScDefn) semi
  eof
  return $ foldl1 (++) defs

-- Supercombinator parser
pScDefn :: SParser Program
pScDefn = do
  name       <- pVariable
  parameters <- many pVariable
  expr       <- reservedOp "=" *> pExpr
  return [(name, parameters, expr)]

pExpr :: SParser Expr
pExpr = pLet <|> pCase <|> pLam <|> pAExpr

-- Lambda ~ \x y -> expr
pLam :: SParser Expr
pLam = do
  params <- reservedOp "\"" *> many pVariable
  expr   <- reservedOp "->" *> pExpr
  return $ ELam params expr

-- Case ~ case expr of [alter]
pCase :: SParser Expr
pCase = do
  expr   <- reserved "case" *> pExpr <* reserved "of"
  alters <- sepEndBy1 pCaseAlter semi
  return $ ECase expr alters

-- Case alter ~ data constructor [arg]* -> expr
pCaseAlter :: SParser (Int, [String], Expr)
pCaseAlter = do
  EConst tag arity <- pDataConst
  args             <- many pVariable
  expr             <- reservedOp "->" *> pExpr
  return (tag, args, expr)

-- Let ~ (let | letrc) bindings in expr
pLet :: SParser Expr
pLet = do
  isRec    <- False <$ reserved "let" <|> True <$ reserved "letrec"
  bindings <- sepEndBy1 pLetBindings semi
  expr     <- reserved "in" *> pExpr
  return $ ELet isRec bindings expr

-- Let bindings ~ name = expr
pLetBindings :: SParser (String, Expr)
pLetBindings = do
  name <- pVariable
  expr <- reserved "=" *> pExpr
  return (name, expr)

-- Arithmetic parser
pAExpr :: SParser Expr
pAExpr = Ex.buildExpressionParser operators pTerms

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

-- Binary operator currying
binOp :: Expr -> Expr -> Expr -> Expr
binOp op left right = foldl1 EAp [op, left, right]

-- Application -> atom [atom]*
pTerms :: SParser Expr
pTerms = makeSpine <$> many1 pAtom where makeSpine = foldl1 EAp

-- Atom -> data constructor | var | num | ( expr )
pAtom :: SParser Expr
pAtom =
  parens pExpr
    <|> (EVar <$> pVariable)
    <|> (ENum . fromIntegral <$> natural)
    <|> pDataConst
    <?> "constructor, variable, number, or parenthesized expression"

pDataConst :: SParser Expr
pDataConst = do
  name <- pUppercased
  PEInfo { currentTag = currentTag, tName = tName } <- getState
  case M.lookup name tName of
    Nothing           -> fail $ "cannot find data constructor " ++ name
    Just (tag, arity) -> return $ EConst tag arity

-- Data type parser
pScData :: SParser Program
pScData = do
  name  <- reserved "data" *> pUppercased
  types <- many pVariable <* reservedOp "="
  sepEndBy pScDataConst (reservedOp "|")

-- Data constructor parser
pScDataConst :: SParser ScDefn
pScDataConst = do
  name                        <- pUppercased
  arity                       <- many pVariable
  PEInfo { currentTag = tag } <- getState
  modifyState $ peTagInc name (length arity)
  return $ buildScDc tag (length arity)

-- MQ 2018-10-24
-- Max number of arguments is 26
buildScDc :: Int -> Int -> ScDefn
buildScDc tag 0 = (show tag, [], EConst tag 0)
buildScDc tag n = (show tag, params, expr)
 where
  params = [ [chr (96 + i)] | i <- [1 .. n] ]
  expr   = foldl fn (EConst tag n) params
  fn e a = EAp e (EVar a)

-- data declaration has to be declared before using
parse :: String -> Either ParseError Program
parse = runP pProgram peInfo "mhaskell"
