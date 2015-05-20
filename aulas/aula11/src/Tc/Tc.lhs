> {-# LANGUAGE TupleSections #-}

Type inference engine
==============

> module Tc.Tc where

> import Control.Monad.Except
> import Control.Monad.Identity
> import Control.Monad.Reader
> import Control.Monad.State

> import Data.Map (Map)
> import qualified Data.Map as Map
> import Data.Set(Set)
> import qualified Data.Set as Set

> import Data.Syntax

> import Tc.TcMonad
> import Tc.Subst

> inference :: Ctx -> Term -> Either String Tau
> inference ctx t = either Left (Right . fst) (fst $ runTcM ctx (infer t))

> infer :: Term -> TcM (Tau , Subst)
> infer (Const c) = inferConst c
> infer (Var v)
>       = do
>           t <- instantiate =<< lookupEnv v
>           return (t, nullSubst)
> infer (Abs v t)
>       = do
>           v' <- fresh
>           (tau,s) <- local (insertEnv v (Forall [] v')) (infer t)
>           return (TArrow v' tau, s)
> infer (App l r)
>       = do
>           v' <- fresh
>           (t , s) <- infer l
>           (t' , s') <- infer r
>           s'' <- unify t (TArrow t' v')
>           let s1 =  s'' `compose` s' `compose` s
>           return (apply s1 v', s1)
> infer (Let v e' e)
>       = do
>           (t , s) <- infer e'
>           sig <- generalize t
>           (t' , s') <- local (insertEnv v sig) (infer e)
>           return (t' , s' `compose` s)


Generalization and instantiation

> generalize :: Tau -> TcM Sigma
> generalize t
>         = do
>             cs <- asks fv
>             let vs = Set.toList $ fv t `Set.difference` cs
>             return (Forall vs t)

> instantiate :: Sigma -> TcM Tau
> instantiate (Forall vs t)
>         = do
>             vs' <- mapM (const fresh) vs
>             let s = Map.fromList $ zip vs vs'
>             return (apply s t)

> inferConst :: Lit -> TcM (Tau , Subst)
> inferConst (ILit _) = return (TInt , nullSubst)
> inferConst (BLit _) = return (TBool , nullSubst)
> inferConst (CLit _) = return (TChar , nullSubst)

