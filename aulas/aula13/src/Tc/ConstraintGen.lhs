Constraint generation for coreML
================================

> module Tc.ConstraintGen where

> import Control.Monad.Identity
> import Control.Monad.Except
> import Control.Monad.RWS
> import Control.Monad.Writer

> import Data.List (intersperse)
> import Data.Map(Map)
> import qualified Data.Map as Map

> import Data.Syntax

> import Tc.TcMonad
> import Tc.TcSubst

> import Utils.PPrint 


Constraint generation
---------------------

> generate :: Ctx -> Term -> Either String (Tau , [Constraint])
> generate ctx t = runTcM ctx (gen t)

> gen :: Term -> TcM Tau
> gen (Const c) = inferConst c
> gen (Var v) = lookupEnv v
> gen (Abs n t)
>          = do
>             v <- fresh
>             let tau = (Forall []) v
>             tau' <- local (insertEnv n tau)(gen t)
>             return (TArrow v tau')
> gen (App l r)
>          = do
>             v <- fresh
>             t <- gen l
>             t' <- gen r
>             addConstr t (TArrow t' v)
>             return v
> gen (Let b e')
>          = do
>             t <- gen (term b)
>             sig <- generalize t
>             t' <- local (insertEnv (name b) sig) (gen e')
>             return t'

Solver interface
----------------

> solver :: [Constraint] -> Tau -> Either String (Sigma , [Constraint])
> solver cs tau = runTcM (Ctx Map.empty) (solve cs tau)

> solve :: [Constraint] -> Tau -> TcM Sigma
> solve cs tau = do
>                   s <- unify cs
>                   generalize (apply s tau)

Inference algorithm
--------------------

> infer :: Ctx -> Term -> Either String (Sigma , [Constraint])
> infer ctx t = do
>                 (tau,cs) <- generate ctx t
>                 solver cs tau 

> inferConst :: Lit -> TcM Tau
> inferConst (ILit _) = return tInt
> inferConst (CLit _) = return tChar



