module TypeInference
  ( inferProgram
  )
where

import           Control.Monad.State
import           Control.Monad.Except
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.List                      ( mapAccumL
                                                , find
                                                )
import           Type
import           LambdaLift                     ( freeVarsExpr )

data MType =
  TVar String
  | TConst String
  | TFun MType MType
  deriving (Show, Eq, Ord)

data PType = Forall (Set String) MType deriving (Show, Eq, Ord)

data Env = Env (Map String PType)

data Unique = Unique { count :: Int }

data TypeError =
  UnificationFail MType MType
  | InfiniteType String MType
  | UnboundVariable String
  deriving Show

type Infer = ExceptT TypeError (State Unique)

data Subst = Subst (Map String MType)

infer :: Env -> Expr -> Infer (Subst, MType)
infer env expr = case expr of
  EVar x              -> inferVar env x
  ENum n              -> inferNum n
  EAp    f      e     -> inferApp env f e
  ELam   params body  -> inferLam env params body
  EConst tag    arity -> inferEConst tag

inferEConst :: Int -> Infer (Subst, MType)
inferEConst tag = return (nullSubst, boolType)

inferLam :: Env -> [String] -> Expr -> Infer (Subst, MType)
inferLam env [p] body = do
  tau <- fresh
  let sigma = Forall S.empty tau
      env1  = extendEnv env (p, sigma)
  (s, tau1) <- infer env1 body
  return (s, TFun (applySubst s tau) tau1)
inferLam env (p : ps) body = do
  tau <- fresh
  let sigma = Forall S.empty tau
      env1  = extendEnv env (p, sigma)
  (s, tau1) <- inferLam env1 ps body
  return (s, TFun (applySubst s tau) tau1)

inferNum :: Int -> Infer (Subst, MType)
inferNum n = return (nullSubst, intType)

inferVar :: Env -> String -> Infer (Subst, MType)
inferVar env name = do
  sigma <- lookupEnv env name
  tau   <- instantiate sigma
  return (nullSubst, tau)

inferApp :: Env -> Expr -> Expr -> Infer (Subst, MType)
inferApp env f x = do
  (s1, fTau) <- infer env f
  (s2, xTau) <- infer (applySubst s1 env) x
  fxTau      <- fresh
  s3         <- unify (applySubst s2 fTau, TFun xTau fxTau)
  let s = s3 `compose` s2 `compose` s1
  pure (s, applySubst s3 fxTau)

unify :: (MType, MType) -> Infer Subst
unify (m1, m2) = case (m1, m2) of
  (TFun a b, TFun x y)          -> unifyBinary (a, b) (x, y)
  (TVar v  , x       )          -> v `bindVariableTo` x
  (x       , TVar v  )          -> v `bindVariableTo` x
  (TConst a, TConst b) | a == b -> return nullSubst
  (a, b)                        -> throwError $ UnificationFail a b

unifyBinary :: (MType, MType) -> (MType, MType) -> Infer Subst
unifyBinary (a, b) (x, y) = do
  s1 <- unify (a, x)
  s2 <- unify (applySubst s1 b, applySubst s1 y)
  return (s1 `compose` s2)

-- Helper
compose :: Subst -> Subst -> Subst
s@(Subst s1) `compose` (Subst s2) =
  Subst $ M.map (applySubst s) s2 `M.union` s1

bindVariableTo :: String -> MType -> Infer Subst
bindVariableTo name (TVar v) | boundToSelf = return nullSubst
  where boundToSelf = name == v
bindVariableTo name mType | name `occursIn` mType = throwError
  $ InfiniteType name mType
  where n `occursIn` ty = n `S.member` ftv ty
bindVariableTo name mType = pure (Subst (M.singleton name mType))

nullSubst :: Subst
nullSubst = Subst M.empty

emptyEnv :: Env
emptyEnv = Env M.empty

lookupEnv :: Env -> String -> Infer PType
lookupEnv (Env env) name = case M.lookup name env of
  Just x  -> return x
  Nothing -> throwError $ UnboundVariable (show name)

extendEnv :: Env -> (String, PType) -> Env
extendEnv (Env env) (x, s) = Env $ M.insert x s env

instantiate :: PType -> Infer MType
instantiate (Forall qs t) = do
  subst <- substituteAllWithFresh qs
  return (applySubst subst t)

substituteAllWithFresh :: Set String -> Infer Subst
substituteAllWithFresh xs = do
  let freshSubstActions = M.fromSet (const fresh) xs
  freshSubsts <- sequenceA freshSubstActions
  pure (Subst freshSubsts)

fresh :: Infer MType
fresh = do
  s <- get
  put s { count = count s + 1 }
  return $ TVar (letters !! count s)

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

generalize :: Env -> MType -> PType
generalize env mType = Forall qs mType
  where qs = ftv mType `S.difference` ftv env

class Substitutable a where
  applySubst  :: Subst -> a -> a
  ftv   :: a -> Set String

instance Substitutable MType where
  applySubst  _ (TConst a)       = TConst a
  applySubst  (Subst s1) t@(TVar a)     = M.findWithDefault t a s1
  applySubst s (TFun a b) = TFun (applySubst s a) (applySubst s b)

  ftv (TConst _)         = S.empty
  ftv (TVar a)       = S.singleton a
  ftv (TFun a b) = ftv a `S.union` ftv b

instance Substitutable PType where
  applySubst  s@(Subst s1) (Forall as t)   = Forall as $ applySubst  (Subst s2) t
                            where s2 = foldr M.delete s1 as
  ftv (Forall as t) = ftv t `S.difference` as

instance Substitutable a => Substitutable [a] where
  applySubst  = fmap . applySubst
  ftv   = foldr (S.union . ftv) S.empty

instance Substitutable Env where
  applySubst  s (Env env) = Env $ M.map (applySubst  s) env
  ftv (Env env) = ftv $ M.elems env

initUnique :: Unique
initUnique = Unique { count = 0 }

runInfer :: Infer (Subst, MType) -> Either TypeError (Subst, MType)
runInfer m = case evalState (runExceptT m) initUnique of
  Left  err -> Left err
  Right res -> Right res

inferProgram :: Program -> (Env, [PType])
inferProgram program = mapAccumL (inferSc program)
                                 env
                                 (scEtaConversion program)
  where env = Env $ M.fromList primOps

inferSc :: Program -> Env -> ScDefn -> (Env, PType)
inferSc program env (name, [], body) = (env2, ptype)
 where
  (freeScs, _)   = freeVarsExpr S.empty body
  env1           = calcEnv program env (S.toList freeScs)
  (subst, mtype) = case runInfer $ infer env body of
    Right e@(subst, mtype) -> e
    Left  e                -> error $ show e
  ptype = generalize emptyEnv mtype
  env2  = extendEnv env1 (name, ptype)

calcEnv :: Program -> Env -> [String] -> Env
calcEnv _ env [] = env
calcEnv program env (freeName : fs)
  | isSuperCombinartor program freeName = calcEnv program env1 fs
  | otherwise = error $ show freeName ++ " is not a supercombinator"
 where
  (Just sc) = find (\(scName, _, _) -> scName == freeName) program
  (env1, _) = inferSc program env sc

isSuperCombinartor :: Program -> String -> Bool
isSuperCombinartor program name =
  name `elem` map (\(scName, _, _) -> scName) program

-- Eta conversion for super combinators
scEtaConversion :: Program -> Program
scEtaConversion program =
  [ (name, [], makeBody params body) | (name, params, body) <- program ]
 where
  makeBody []     body = body
  makeBody params body = ELam params body

primOps =
  [ ("negate", negateScheme)
  , ("+"     , arithScheme)
  , ("-"     , arithScheme)
  , ("*"     , arithScheme)
  , ("/"     , arithScheme)
  , (">"     , compareScheme)
  , (">="    , compareScheme)
  , ("<"     , compareScheme)
  , ("<="    , compareScheme)
  ]

arithScheme = Forall S.empty (TFun intType (TFun intType intType))
compareScheme = Forall S.empty (TFun intType (TFun intType boolType))
boolScheme = Forall S.empty boolType
negateScheme = Forall S.empty (TFun intType intType)

intType = TConst "Integer"
boolType = TConst "Bool"
