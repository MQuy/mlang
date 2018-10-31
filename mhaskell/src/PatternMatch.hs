module PatternMatch where

import           Data.List                      ( partition )
import qualified Data.Map                      as M
import           Data.Set                       ( toList
                                                , fromList
                                                )
import           Data.Char                      ( chr )
import           Type

type ScPattern = M.Map String PProgram

matching :: PProgram -> Program
matching scs = [ prepare nsc | nsc <- nscs ]
 where
  nscs = map snd $ M.toList (foldl group M.empty scs)
  group m e@(name, params, body) = M.insertWith (++) name [e] m

prepare :: [PScDefn] -> ScDefn
prepare scs = (name, params, match 0 params equations (EConst cExit 0))
 where
  equations     = [ (ps, expr) | (name, ps, expr) <- scs ]
  params        = [ [chr (96 + i)] | i <- [1 .. length ps] ]
  (name, ps, _) = head scs

{-
match us qs E
- us: [u1, u2, ... un]
- qs: [
  ([q11, q12, ... q1n], e1),
  ...
  ([qm1, qm2, ... qmn], em)
]

Rule 1: If q1x -> qmx are variables, we simply remove it
Rule 2: Group those qxy which have a same type (constructor, variable)
  - variable: move to a next ui
  - constructor: base on a number of arity, run match with those arity
-}
type Equation = ([PPattern], Expr)

match :: Int -> [String] -> [Equation] -> Expr -> Expr
match k [] qs def = head ([ expr | (_, expr) <- qs ] ++ [def])
match k (u : us) qs def =
  foldr (matchVarCon k (u : us)) def (mPartition isVar qs)

matchVarCon :: Int -> [String] -> [Equation] -> Expr -> Expr
matchVarCon k (u : us) qs def | isVar (head qs) = matchVar k (u : us) qs def
                              | otherwise       = matchCon k (u : us) qs def

matchVar :: Int -> [String] -> [Equation] -> Expr -> Expr
matchVar k (u : us) qs =
  match k us [ (ps, subst expr v u) | (PVar v : ps, expr) <- qs ]

matchCon :: Int -> [String] -> [Equation] -> Expr -> Expr
matchCon k (u : us) qs def = ECase
  (EVar u)
  (  [ matchClause c k (u : us) (choose c qs) def | c <- cs ]
  ++ [(cOtherwise, [], def)]
  )
  where cs = (toList . fromList) $ map (tagOfConst . headPattern) qs

matchClause
  :: Int -> Int -> [String] -> [Equation] -> Expr -> (Int, [String], Expr)
matchClause c k (u : us) qs def =
  ( c
  , us1
  , match (k1 + k)
          (us1 ++ us)
          [ (ps1 ++ ps, e) | (PConst tag ps1 : ps, e) <- qs ]
          def
  )
 where
  k1  = lengthOfArgs (headPattern $ head qs)
  us1 = [ makeVar (i + k) | i <- [1 .. k1] ]

-- Helper
tagOfConst :: PPattern -> Int
tagOfConst (PConst tag _) = tag
tagOfConst _              = error "cannot happen"

lengthOfArgs :: PPattern -> Int
lengthOfArgs (PConst _ args) = length args
lengthOfArgs _               = error "cannot happen"

choose :: Int -> [Equation] -> [Equation]
choose c qs = [ q | q <- qs, tagOfConst (headPattern q) == c ]

headPattern :: Equation -> PPattern
headPattern (PConst tag args : _, e) = PConst tag args
headPattern _                        = error "cannot happen"

isVar :: Equation -> Bool
isVar (PVar _ : _, _) = True
isVar _               = False

subst :: Expr -> String -> String -> Expr
subst e@(ENum n          ) _ _ = e
subst e@(EConst tag arity) _ _ = e
subst (EVar v) nOld nNew | v == nOld = EVar nNew
                         | otherwise = EVar v
subst (EAp e1 e2) nOld nNew = EAp (subst e1 nOld nNew) (subst e2 nOld nNew)
subst (ECase e alters) nOld nNew = ECase
  (subst e nOld nNew)
  [ (tag, args, subst aExpr nOld nNew) | (tag, args, aExpr) <- alters ]
subst (ELet recursive defs e) nOld nNew = ELet
  recursive
  [ (name, subst dExpr nOld nNew) | (name, dExpr) <- defs ]
  (subst e nOld nNew)
subst (ELam args body) nOld nNew = ELam args (subst body nOld nNew)

makeVar :: Int -> String
makeVar k = "_u" ++ show k

mPartition :: (a -> Bool) -> [a] -> [[a]]
mPartition f []  = []
mPartition f [x] = [[x]]
mPartition f (x : x' : xs) | f x == f x' = tack x (mPartition f (x' : xs))
                           | otherwise   = [x] : mPartition f (x' : xs)

tack :: a -> [[a]] -> [[a]]
tack x xss = (x : head xss) : tail xss
