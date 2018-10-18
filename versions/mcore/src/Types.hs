module Types where

-- A core program is just a list of supercombinators
type Program = [ScDefn]

-- A Supercombinator contains name, its arguments and body
type ScDefn = (String, [String], Expr)

-- The base data structure for the core language
data Expr =
  EVar String
  | ENum Integer
  | EConst Integer Integer
  | EAp Expr Expr
  | ELet
        Bool
        [(String, Expr)]
        Expr
  | ECase
        Expr
        [Alter]
  | ELam [String] Expr
  deriving Show

type Alter = (Integer, [String], Expr)

data Pattern = PCon String | PTag Integer deriving Show

recursive :: Bool
recursive = True

nonRecursive :: Bool
nonRecursive = False

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [ name | (name, _) <- defns ]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [ rhs | (_, rhs) <- defns ]

-- A function to find out whether we have an atomic expression
isAtomicExpr :: Expr -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _        = False

-- Define the Standard Core Prelude
-- I x = x
-- K x y = x
-- K1 x y = y
-- S f g x = f x (g x)
-- compose f g x = f (g x)
-- twice f = compose f f
preludeDefs :: Program
preludeDefs =
  [ ("I" , ["x"]     , EVar "x")
  , ("K" , ["x", "y"], EVar "x")
  , ("K1", ["x", "y"], EVar "y")
  , ( "S"
    , ["f", "g", "x"]
    , EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))
    )
  , ("compose", ["f", "g", "x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x")))
  , ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
  ]
