Constraint generation for coreML
================================

> module Tc.ConstraintGen where

> import Control.Monad.Identity
> import Control.Monad.Reader
> import Control.Monad.State
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

> generate :: Term -> TcM Tau
> generate (Const c) = inferConst c
> generate (Var v) = lookupEnv v
> generate (Abs n t)
>              = do
>                 v <- fresh
>                 let tau = (Forall []) v
>                 tau' <- local (insertEnv n tau)(generate t)
>                 return (TArrow v tau')
> generate (App l r)
>              = do
>                 v <- fresh
>                 t <- generate l
>                 t' <- generate r
>                 addConstr t (TArrow t' v)
>                 return v
> generate (Let b e')
>              = do
>                 t <- generate (term b)
>                 sig <- generalize t
>                 t' <- local (insertEnv (name b) sig) (generate e')
>                 return t'


> inferConst :: Lit -> TcM Tau
> inferConst (ILit _) = return TInt
> inferConst (CLit _) = return TChar
> inferConst (BLit _) = return TBool


