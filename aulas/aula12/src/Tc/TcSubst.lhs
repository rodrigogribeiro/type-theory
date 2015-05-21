Type substitutions and related operations
===========================

> module Tc.TcSubst where

> import Control.Monad.Reader

> import Data.Map (Map)
> import qualified Data.Map as Map
> import Data.Set(Set)
> import qualified Data.Set as Set

> import Data.Syntax

> import Tc.TcMonad

Substitution definition
-----------------------

> newtype Subst = Subst { unSubst :: Map Tyvar Tau }
>                 deriving (Eq, Ord)

> nullSubst :: Subst
> nullSubst = Subst Map.empty

> class Substitutable a where
>    apply :: Subst -> a -> a
>    fv    :: a -> Set Tyvar

> instance Substitutable a => Substitutable [a] where
>    apply s = map (apply s)
>    fv      = foldr (Set.union . fv) Set.empty

> instance Substitutable Tau where
>    apply s t@(TyVar v)  = Map.findWithDefault t v (unSubst s)
>    apply s (TArrow l r) = TArrow (apply s l) (apply s r)
>    apply s t            = t

>    fv (TyVar v)    = Set.singleton v
>    fv (TArrow l r) = fv l `Set.union` fv r
>    fv _            = Set.empty

> instance Substitutable Constraint where
>    apply s (Constraint (t,t')) = Constraint (apply s t, apply s t')
>    fv (Constraint (t,t')) = fv t `Set.union` fv t'

> instance Substitutable Sigma where
>    apply s (Forall vs tau) = Forall vs (apply s' tau)
>                    where s' = Subst $ foldr Map.delete (unSubst s) vs
>    fv (Forall vs tau) = fv tau `Set.difference` (Set.fromList vs)

> instance Substitutable Ctx where
>    apply s = Ctx . Map.map (apply s) . unCtx
>    fv = Map.foldr (Set.union . fv) Set.empty .unCtx

Substitution composition
-------------------------

> compose :: Subst -> Subst -> Subst
> ss@(Subst s) `compose` (Subst s') = Subst $ Map.map (apply ss) s' `Map.union` s


Unification algorithm
---------------------

> unify :: [Constraint] -> TcM Subst
> unify [] = return nullSubst
> unify (c : cs)
>       = do
>           s <- unifyConstr c
>           s' <- unify (apply s cs)
>           return (s' `compose` s)

> unifyConstr :: Constraint -> TcM Subst
> unifyConstr (Constraint (t,t')) = unifyTau t t'

> unifyTau :: Tau -> Tau -> TcM Subst
> unifyTau (TyVar v) t = varBind v t
> unifyTau t (TyVar v) = varBind v t
> unifyTau (TArrow l r) (TArrow l' r')
>          = unify [Constraint (l,l'), Constraint (r,r')]
> unifyTau t t'
>          | t == t'   = return nullSubst
>          | otherwise = unifyError t t'

> varBind :: Tyvar -> Tau -> TcM Subst
> varBind v t
>         | v `Set.member` fv t = occursCheckError v t
>         | otherwise           = return $ Subst (Map.singleton v t)



Free variables from context
---------------------------

> freeVars :: TcM (Set Tyvar)
> freeVars = asks fv

> lookupEnv :: Name -> TcM Tau
> lookupEnv n
>        = do
>            ctx <- asks unCtx
>            case Map.lookup n ctx of
>                 Nothing -> undefinedVar n
>                 Just sig  -> instantiate sig


Generalization and instantiation
--------------------------------

> instantiate :: Sigma -> TcM Tau
> instantiate (Forall vs tau)
>             = do
>                 vs' <- mapM (const fresh) vs
>                 let s = Subst $ Map.fromList $ zip vs vs'
>                 return (apply s tau)

> generalize :: Tau -> TcM Sigma
> generalize tau
>             = do
>                 vs <- freeVars
>                 let qs = fv tau `Set.difference` vs
>                 return (Forall (Set.toList qs) tau)

