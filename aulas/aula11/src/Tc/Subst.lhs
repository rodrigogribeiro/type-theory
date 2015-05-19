Definition of type substitutions and unification algorithm
----------------------------------------------

> module Tc.Subst where

> import Control.Monad.Except

> import Data.Map(Map)
> import qualified Data.Map as Map
> import Data.Set(Set)
> import qualified Data.Set as Set
> import Data.Syntax

> import Tc.TcMonad

Definition of substitution

> type Subst = Map Tyvar Tau

Identity substitution

> nullSubst :: Subst
> nullSubst = Map.empty

Substitution application

> class Substitutable a where
>    apply :: Subst -> a -> a
>    fv    :: a -> Set Tyvar

> instance Substitutable a => Substitutable [a] where
>    apply s = map (apply s)
>    fv      = foldr (Set.union . fv) Set.empty

> instance Substitutable Tau where
>    apply s t@(TyVar v)  = maybe t id (Map.lookup v s)
>    apply s (TArrow l r) = TArrow (apply s l)
>                                  (apply s r)
>    apply s t            = t

>    fv (TyVar v)    = Set.singleton v
>    fv (TArrow l r) = fv l `Set.union` fv r
>    fv _            = Set.empty

> instance Substitutable Sigma where
>    apply s (Forall vs t) = Forall vs $ apply s' t
>                            where
>                              s' = foldr Map.delete s vs

>    fv (Forall vs t) = fv t `Set.difference` Set.fromList vs

> instance Substitutable Ctx where
>    apply s = Ctx . Map.map (apply s) . unCtx
>    fv = fv . Map.elems . unCtx

> compose :: Subst -> Subst -> Subst
> s `compose` s' = Map.map (apply s) s' `Map.union` s

Unification
-----------

> occursCheck :: Substitutable a => Tyvar -> a -> Bool
> occursCheck v t = v `Set.member` fv t

> unify :: Tau -> Tau -> TcM Subst
> unify (TyVar v) t = varBind v t
> unify t (TyVar v) = varBind v t
> unify (TArrow l r) (TArrow l' r')
>       = do
>           s <- unify l l'
>           s' <- unify (apply s r) (apply s r')
>           return (compose s' s)
> unify t t'
>       | t == t' = return nullSubst
>       | otherwise = unifyError t t'

> varBind :: Tyvar -> Tau -> TcM Subst
> varBind v t
>         | occursCheck v t = occursCheckError v t
>         | otherwise       = return (Map.singleton v t)

Error messages
--------------

> occursCheckError :: Tyvar -> Tau -> TcM a
> occursCheckError v t = throwError msg
>                  where
>                     msg = "The variable " ++ (show $ pprint v) ++
>                            "\noccurs in type " ++ (show $ pprint t)


> unifyError :: Tau -> Tau -> TcM a
> unifyError t t' = throwError ("The types\n" ++ (show $ pprint t) ++
>                               "\nand\n" ++ (show $ pprint t') ++
>                               "\naren't unifiable!")