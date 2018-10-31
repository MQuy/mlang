module LambdaLift
  ( liftLambda
  )
where

import qualified Data.Set                      as S
import qualified Data.Map                      as M
import           Data.Char                      ( isDigit )
import           Data.List                      ( foldl'
                                                , mapAccumL
                                                , isPrefixOf
                                                )
import           Type

type FVProgram = AProgram String (S.Set String)
type FVExpr = Annotated String (S.Set String)
type FVAlt = AAlter String (S.Set String)

-- Free variable
freeVars :: Program -> FVProgram
freeVars program =
  [ (name, args, freeVarsExpr (S.fromList args) body)
  | (name, args, body) <- program
  ]

freeVarsExpr :: S.Set String -> Expr -> FVExpr
freeVarsExpr _ (ENum n) = (S.empty, ANum n)
freeVarsExpr env (EVar v) | v `S.member` env = (S.singleton v, AVar v)
                          | otherwise        = (S.empty, AVar v)
freeVarsExpr env (EConst tag arity) = (S.empty, AConst tag arity)
freeVarsExpr env (EAp e1 e2) =
  (freeVarsOf e11 `S.union` freeVarsOf e21, AAp e11 e21)
 where
  e11 = freeVarsExpr env e1
  e21 = freeVarsExpr env e2
freeVarsExpr env (ELam args body) =
  (freeVarsOf body1 `S.difference` argSet, ALam args body1)
 where
  argSet = S.fromList args
  env1   = env `S.union` argSet
  body1  = freeVarsExpr env1 body
freeVarsExpr env (ELet recursive defs body) =
  (env1, ALet recursive defs1 body1)
 where
  binders   = bindersOf defs
  binderSet = S.fromList binders
  bodyEnv   = env `S.union` binderSet
  defEnv | recursive = bodyEnv
         | otherwise = env
  exprs        = map (freeVarsExpr defEnv) (rhssOf defs)
  defs1        = zip binders exprs
  freeInValues = S.unions (map freeVarsOf exprs)
  defsFree | recursive = freeInValues `S.difference` binderSet
           | otherwise = freeInValues
  body1    = freeVarsExpr bodyEnv body
  bodyFree = freeVarsOf body1 `S.difference` binderSet
  env1     = defsFree `S.union` bodyFree
freeVarsExpr env (ECase e alters) = (env1, ACase e1 alters1)
 where
  e1 = freeVarsExpr env e
  alters1 =
    [ (tag, args, freeVarsExpr (env `S.union` S.fromList args) body)
    | (tag, args, body) <- alters
    ]
  env1 = freeVarsOf e1 `S.union` S.unions
    [ freeVarsOf body `S.difference` S.fromList args
    | (tag, args, body) <- alters1
    ]

-- Abstract
abstract :: FVProgram -> Program
abstract program =
  [ (name, args, abstractE body) | (name, args, body) <- program ]

abstractE :: FVExpr -> Expr
abstractE (free, AVar v                  ) = EVar v
abstractE (free, ANum n                  ) = ENum n
abstractE (free, AConst tag arity        ) = EConst tag arity
abstractE (free, AAp e1 e2               ) = EAp (abstractE e1) (abstractE e2)
abstractE (free, ALet recursive defs body) = ELet
  recursive
  [ (name, abstractE body1) | (name, body1) <- defs ]
  (abstractE body)
abstractE (free, ALam args body) = foldl' EAp sc (map EVar fvList)
 where
  fvList = S.toList free
  rhs    = ELam (fvList ++ args) (abstractE body)
  sc     = ELet False [(llName, rhs)] (EVar llName)
abstractE (free, ACase e alters) = ECase
  (abstractE e)
  [ (tag, args, abstractE body) | (tag, args, body) <- alters ]

-- Rename
initialNameSupply = 0

rename :: Program -> Program
rename program = snd $ mapAccumL renameSc initialNameSupply program

renameSc :: Int -> (String, [String], Expr) -> (Int, (String, [String], Expr))
renameSc ns (scName, args, rhs) = (ns2, (scName, args1, rhs1))
 where
  (ns1, args1, env) = newNames ns args
  (ns2, rhs1)       = renameE env ns1 rhs

newNames :: Int -> [String] -> (Int, [String], [(String, String)])
newNames ns names = (ns1, names1, env)
 where
  (ns1, names1) = getNames ns names
  env           = zip names names1

getNames :: Int -> [String] -> (Int, [String])
getNames nameSupply prefixes =
  (nameSupply + length prefixes, zipWith makeName prefixes [nameSupply ..])

makeName :: String -> Int -> String
makeName prefix ns = prefix ++ "_" ++ show ns

renameE :: [(String, String)] -> Int -> Expr -> (Int, Expr)
renameE env ns (  EVar v          ) = (ns, EVar (aLookup env v v))
renameE env ns e@(ENum n          ) = (ns, e)
renameE env ns e@(EConst tag arity) = (ns, e)
renameE env ns (  EAp    e1  e2   ) = (ns2, EAp e11 e21)
 where
  (ns1, e11) = renameE env ns e1
  (ns2, e21) = renameE env ns1 e2
renameE env ns (ELam args body) = (ns1, ELam args1 body1)
 where
  (ns1, args1, env1) = newNames ns args
  (ns2, body1)       = renameE (env1 ++ env) ns1 body
renameE env ns (ELet recursive defs body) =
  (ns3, ELet recursive (zip binders1 rhss1) body1)
 where
  binders               = bindersOf defs
  (ns1, body1)          = renameE bodyEnv ns body
  (ns2, binders1, env1) = newNames ns1 binders
  bodyEnv               = env1 ++ env
  (ns3, rhss1)          = mapAccumL (renameE rhsEnv) ns2 (rhssOf defs)
  rhsEnv | recursive = bodyEnv
         | otherwise = env
renameE env ns (ECase e alters) = (ns2, ECase e1 alters1)
 where
  (ns1, e1     ) = renameE env ns e
  (ns2, alters1) = mapAccumL (renameEAlter env) ns1 alters

renameEAlter :: [(String, String)] -> Int -> EAlter -> (Int, EAlter)
renameEAlter env ns (tag, args, body) = (ns2, (tag, args1, body1))
 where
  (ns1, args1, env1) = newNames ns args
  (ns2, body1)       = renameE (env ++ env1) ns1 body

-- Collect sc
collectScs :: Program -> Program
collectScs = concatMap collectSc

collectSc :: ScDefn -> [ScDefn]
collectSc (name, args, body) = if isAbstractLam body
  then collectLamSc name scs
  else (name, args, body1) : scs
  where (scs, body1) = collectScE body

-- If sc's body is lambda abstraction, simply, replacing it
-- f x = \y -> x + y => f x y = x + y
collectLamSc :: String -> [ScDefn] -> [ScDefn]
collectLamSc name scs = (name, args1, body) : tail scs
  where (name1, args1, body) = head scs

collectScE :: Expr -> ([ScDefn], Expr)
collectScE (ENum n   ) = ([], ENum n)
collectScE (EVar v   ) = ([], EVar v)
collectScE (EAp e1 e2) = (scs1 ++ scs2, EAp e11 e21)
 where
  (scs1, e11) = collectScE e1
  (scs2, e21) = collectScE e2
collectScE (ELam args body) = (scs, ELam args body1)
  where (scs, body1) = collectScE body
collectScE (EConst tag arity) = ([], EConst tag arity)
collectScE (ECase  e   alts ) = (scsE ++ scsAlts, ECase e1 alts1)
 where
  (scsE   , e1   ) = collectScE e
  (scsAlts, alts1) = mapAccumL collectScAlt [] alts
collectScE (ELet recursive defs body) =
  (rhssScs ++ bodyScs ++ localScs, mkELet recursive nonScs1 body1)
 where
  (rhssScs, defs1) = mapAccumL collectScD [] defs
  scs1             = [ (name, rhs) | (name, rhs) <- defs1, isELam rhs ]
  nonScs1          = [ (name, rhs) | (name, rhs) <- defs1, not (isELam rhs) ]
  localScs         = [ (name, args, body) | (name, ELam args body) <- scs1 ]
  (bodyScs, body1) = collectScE body

collectScD scs (name, rhs) = (scs ++ rhsScs, (name, rhs1))
  where (rhsScs, rhs1) = collectScE rhs

collectScAlt :: [ScDefn] -> EAlter -> ([ScDefn], EAlter)
collectScAlt scs (tag, arity, rhs) = (scs ++ scsRhs, (tag, arity, rhs1))
  where (scsRhs, rhs1) = collectScE rhs

-- Helper
freeVarsOf :: FVExpr -> S.Set String
freeVarsOf (env, _) = env

isELam :: Expr -> Bool
isELam (ELam args body) = True
isELam _                = False

mkELet :: Bool -> [EDef] -> Expr -> Expr
mkELet recursive defs body = case defs of
  [] -> body
  _  -> ELet recursive defs body

isAbstractLam :: Expr -> Bool
isAbstractLam (ELet False [(name, ELam _ _)] _) = llName `isPrefixOf` name
isAbstractLam (EAp e (EVar v)) = isAbstractLam e
isAbstractLam _ = False

--

llName = "$lambda"

liftLambda :: Program -> Program
liftLambda program = dataDeclarations
  ++ collectScs (rename $ abstract $ freeVars supercombinators)
 where
  dataDeclarations = filter (\(name, _, _) -> all isDigit name) program
  supercombinators = filter (\(name, _, _) -> not (all isDigit name)) program
