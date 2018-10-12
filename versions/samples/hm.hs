{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HindleyMilner where

import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.String
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

class Pretty a where
  ppr :: a -> Text


newtype Name = Name Text
  deriving (Eq, Ord, Show)

instance IsString Name where
  fromString = Name . T.pack

instance Pretty Name where
  ppr (Name n) = n



data MType =
  TVar Name
  | TFun MType MType
  | TConst Name
  | TList MType
  | TEither MType MType
  | TTuple MType MType
  deriving Show

instance Pretty MType where
  ppr = go False
    where
      go _ (TVar name)   = ppr name
      go _ (TList a)     = "[" <> ppr a <> "]"
      go _ (TEither l r) = "Either " <> ppr l <> " " <> ppr r
      go _ (TTuple a b)  = "(" <> ppr a <> ", " <> ppr b <> ")"
      go _ (TConst name) = ppr name
      go parenthesize (TFun a b)
          | parenthesize = "(" <> lhs <> " → " <> rhs <> ")"
          | otherwise    =        lhs <> " → " <> rhs
          where lhs = go True a
                rhs = go False b

instance IsString MType where
  fromString = TVar . fromString

freeMType :: MType -> Set Name
freeMType = \case
  TVar a      -> [a]
  TFun a b    -> freeMType a <> freeMType b
  TList a     -> freeMType a
  TEither l r -> freeMType l <> freeMType r
  TTuple  a b -> freeMType a <> freeMType b
  TConst _    -> []

instance Substitutable MType where
  applySubst s = \case
      TVar a -> let Subst s' = s
                in M.findWithDefault (TVar a) a s'
      TFun f x    -> TFun (applySubst s f) (applySubst s x)
      TList a     -> TList (applySubst s a)
      TEither l r -> TEither (applySubst s l) (applySubst s r)
      TTuple a b  -> TTuple (applySubst s a) (applySubst s b)
      c@TConst {} -> c


data PType = Forall (Set Name) MType

instance Pretty PType where
  ppr (Forall qs mType) = "∀" <> pprUniversals <> ". " <> ppr mType
    where
      pprUniversals
        | S.null qs = "∅"
        | otherwise = (T.intercalate " " . map ppr . S.toList) qs


freePType :: PType -> Set Name
freePType (Forall qs mType) = freeMType mType `S.difference` qs



instance Substitutable PType where
  applySubst (Subst subst) (Forall qs mType) =
      let qs' = M.fromSet (const ()) qs
          subst' = Subst (subst `M.difference` qs')
      in Forall qs (applySubst subst' mType)


newtype Env = Env (Map Name PType)

instance Pretty Env where
  ppr (Env env) = "Γ = { " <> T.intercalate "\n    , " pprBindings <> " }"
    where
      bindings = M.assocs env
      pprBinding (name, pType) = ppr name <> " : " <> ppr pType
      pprBindings = map pprBinding bindings


freeEnv :: Env -> Set Name
freeEnv (Env env) =
  let allPTypes = M.elems env in S.unions (map freePType allPTypes)

instance Substitutable Env where
  applySubst s (Env env) = Env (M.map (applySubst s) env)


newtype Subst = Subst (Map Name MType)

class Substitutable a where
  applySubst :: Subst -> a -> a

instance (Substitutable a, Substitutable b) => Substitutable (a,b) where
  applySubst s (x,y) = (applySubst s x, applySubst s y)

instance Substitutable Subst where
  applySubst s (Subst target) = Subst (fmap (applySubst s) target)

instance Pretty Subst where
  ppr (Subst s) = "{ " <> T.intercalate "\n, " [ ppr k <> " ––> " <> ppr v | (k,v) <- M.toList s ] <> " }"

instance Semigroup Subst where
  (<>) subst1 subst2 = Subst (s1 `M.union` s2)
    where
      Subst s1 = subst1
      Subst s2 = applySubst subst1 subst2

instance Monoid Subst where
  mappend subst1 subst2 = Subst (s1 `M.union` s2)
    where
      Subst s1 = subst1
      Subst s2 = applySubst subst1 subst2

  mempty = Subst M.empty

newtype Infer a = Infer (ExceptT InferError (State [Name]) a)
  deriving (Functor, Applicative, Monad)

data InferError =
    CannotUnify MType MType
  | OccursCheckFailed Name MType
  | UnknownIdentifier Name
  deriving Show

instance Pretty InferError where
  ppr = \case
      CannotUnify t1 t2 ->
          "Cannot unify " <> ppr t1 <> " with " <> ppr t2
      OccursCheckFailed name ty ->
          "Occurs check failed: " <> ppr name <> " already appears in " <> ppr ty
      UnknownIdentifier name ->
          "Unknown identifier: " <> ppr name

runInfer :: Infer a -> Either InferError a
runInfer (Infer inf) = evalState (runExceptT inf)
                                 (map Name (infiniteSupply alphabet))
 where
  alphabet = map T.singleton ['a' .. 'z']

  -- [a, b, c] ==> [a,b,c, a1,b1,c1, a2,b2,c2, …]
  infiniteSupply supply = supply <> addSuffixes supply (1 :: Integer)
   where
    addSuffixes xs n = map (\x -> addSuffix x n) xs <> addSuffixes xs (n + 1)
    addSuffix x n = x <> T.pack (show n)

throw :: InferError -> Infer a
throw = Infer . throwE


unify :: (MType, MType) -> Infer Subst
unify = \case
  (TFun a b, TFun x y)          -> unifyBinary (a, b) (x, y)
  (TVar v  , x       )          -> v `bindVariableTo` x
  (x       , TVar v  )          -> v `bindVariableTo` x
  (TConst a, TConst b) | a == b -> pure mempty
  (TList a    , TList b    )    -> unify (a, b)
  (TEither a b, TEither x y)    -> unifyBinary (a, b) (x, y)
  (TTuple  a b, TTuple x y )    -> unifyBinary (a, b) (x, y)
  (a          , b          )    -> throw (CannotUnify a b)
 where

    -- Unification of binary type constructors, such as functions and Either.
    -- Unification is first done for the first operand, and assuming the
    -- required substitution, for the second one.
  unifyBinary :: (MType, MType) -> (MType, MType) -> Infer Subst
  unifyBinary (a, b) (x, y) = do
    s1 <- unify (a, x)
    s2 <- unify (applySubst s1 (b, y))
    pure (s1 <> s2)


bindVariableTo :: Name -> MType -> Infer Subst

bindVariableTo name (TVar v) | boundToSelf = pure mempty
  where boundToSelf = name == v

bindVariableTo name mType | name `occursIn` mType = throw
  (OccursCheckFailed name mType)
  where n `occursIn` ty = n `S.member` freeMType ty

bindVariableTo name mType = pure (Subst (M.singleton name mType))


data Exp = ELit Lit          -- ^ True, 1
         | EVar Name         -- ^ @x@
         | EApp Exp Exp      -- ^ @f x@
         | EAbs Name Exp     -- ^ @λx. e@
         | ELet Name Exp Exp -- ^ @let x = e in e'@ (non-recursive)
         deriving Show

data Lit = LBool Bool
         | LInteger Integer
         deriving Show

instance Pretty Exp where
    ppr (ELit lit) = ppr lit

    ppr (EVar name) = ppr name

    ppr (EApp f x) = pprApp1 f <> " " <> pprApp2 x
      where
        pprApp1 = \case
            eLet@ELet{} -> "(" <> ppr eLet <> ")"
            eLet@EAbs{} -> "(" <> ppr eLet <> ")"
            e -> ppr e
        pprApp2 = \case
            eApp@EApp{} -> "(" <> ppr eApp <> ")"
            e -> pprApp1 e

    ppr x@EAbs{} = pprAbs True x
      where
        pprAbs True  (EAbs name expr) = "λ" <> ppr name <> pprAbs False expr
        pprAbs False (EAbs name expr) = " " <> ppr name <> pprAbs False expr
        pprAbs _ expr                 = ". " <> ppr expr

    ppr (ELet name value body) =
        "let " <> ppr name <> " = " <> ppr value <> " in " <> ppr body

instance Pretty Lit where
    ppr = \case
        LBool    b -> showT b
        LInteger i -> showT i
      where
        showT :: Show a => a -> Text
        showT = T.pack . show

instance IsString Exp where
    fromString = EVar . fromString


fresh :: Infer MType
fresh = drawFromSupply >>= \case
  Right name -> pure (TVar name)
  Left  err  -> throw err
 where

  drawFromSupply :: Infer (Either InferError Name)
  drawFromSupply = Infer
    (do
      s : upply <- lift get
      lift (put upply)
      pure (Right s)
    )

extendEnv :: Env -> (Name, PType) -> Env
extendEnv (Env env) (name, pType) = Env (M.insert name pType env)

infer :: Env -> Exp -> Infer (Subst, MType)
infer env = \case
  ELit lit    -> inferLit lit
  EVar name   -> inferVar env name
  EApp f x    -> inferApp env f x
  EAbs x e    -> inferAbs env x e
  ELet x e e' -> inferLet env x e e'


inferLit :: Lit -> Infer (Subst, MType)
inferLit lit = pure (mempty, TConst litTy)
 where
  litTy = case lit of
    LBool{}    -> "Bool"
    LInteger{} -> "Integer"

inferVar :: Env -> Name -> Infer (Subst, MType)
inferVar env name = do
  sigma <- lookupEnv env name -- x:σ ∈ Γ
  tau   <- instantiate sigma    -- τ = instantiate(σ)
                              -- ------------------
  pure (mempty, tau)          -- Γ ⊢ x:τ

lookupEnv :: Env -> Name -> Infer PType
lookupEnv (Env env) name = case M.lookup name env of
  Just x  -> pure x
  Nothing -> throw (UnknownIdentifier name)


instantiate :: PType -> Infer MType
instantiate (Forall qs t) = do
  subst <- substituteAllWithFresh qs
  pure (applySubst subst t)
 where
  substituteAllWithFresh :: Set Name -> Infer Subst
  substituteAllWithFresh xs = do
    let freshSubstActions = M.fromSet (const fresh) xs
    freshSubsts <- sequenceA freshSubstActions
    pure (Subst freshSubsts)

inferApp
  :: Env
  -> Exp -- ^ __f__ x
  -> Exp -- ^ f __x__
  -> Infer (Subst, MType)
inferApp env f x = do
  (s1, fTau) <- infer env f                         -- f : fτ
  (s2, xTau) <- infer (applySubst s1 env) x         -- x : xτ
  fxTau      <- fresh                                    -- fxτ = fresh
  s3         <- unify (applySubst s2 fTau, TFun xTau fxTau) -- unify (fτ, xτ → fxτ)
  let s = s3 <> s2 <> s1                            -- --------------------
  pure (s, applySubst s3 fxTau)                     -- f x : fxτ


inferAbs
  :: Env
  -> Name -- ^ λ__x__. e
  -> Exp  -- ^ λx. __e__
  -> Infer (Subst, MType)
inferAbs env x e = do
  tau <- fresh                           -- τ = fresh
  let sigma = Forall [] tau              -- σ = ∀∅. τ
      env'  = extendEnv env (x, sigma)    -- Γ, x:σ …
  (s, tau') <- infer env' e              --        … ⊢ e:τ'
                                         -- ---------------
  pure (s, TFun (applySubst s tau) tau') -- λx.e : τ→τ'

inferLet
  :: Env
  -> Name -- ^ let __x__ = e in e'
  -> Exp -- ^ let x = __e__ in e'
  -> Exp -- ^ let x = e in __e'__
  -> Infer (Subst, MType)
inferLet env x e e' = do
  (s1, tau) <- infer env e              -- Γ ⊢ e:τ
  let env'  = applySubst s1 env
  let sigma = generalize env' tau       -- σ = gen(Γ,τ)
  let env'' = extendEnv env' (x, sigma) -- Γ, x:σ
  (s2, tau') <- infer env'' e'          -- Γ ⊢ …
                                        -- --------------------------
  pure (s2 <> s1, tau')                 --     … let x = e in e' : τ'


generalize :: Env -> MType -> PType
generalize env mType = Forall qs mType
  where qs = freeMType mType `S.difference` freeEnv env

showType
  :: Env    -- ^ Starting environment, e.g. 'prelude'.
  -> Exp    -- ^ Expression to typecheck
  -> IO ()   -- ^ Text representation of the result. Contains an error
                   --   message on failure.
showType env expr =
  case
      ( runInfer
        . fmap (generalize (Env mempty) . uncurry applySubst)
        . infer env
        )
        expr
    of
      Left _ -> return ()
      Right ty ->
        (T.putStr . ppr) expr >> T.putStr " :: " >> (T.putStrLn . ppr) ty
