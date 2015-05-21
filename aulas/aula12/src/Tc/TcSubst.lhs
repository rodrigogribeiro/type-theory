Type substitutions and related operations
===========================

> module Tc.TcSubst where

> import Data.Map (Map)
> import qualified Data.Map as Map
> import Data.Set(Set)
> import qualified Data.Set as Set

> import Data.Syntax

> import Tc.TcMonad

Substitution definition
-----------------------

> newtype Subst = Subst { unSubst :: Map Name Tau }
>                 deriving (Eq, Ord)

> class Substitutable a where
>    apply :: Subst -> a -> a
>    fv    :: a -> Set Name

> instance Substitutable a => Substitutable [a] where
>    apply s = map (apply s)
>    fv      = foldr (Set.union . fv) Set.empty

> instance Substitutable Tau where
>    apply s t@(TyVar v)  = Map.findWithDefault t v s
>    apply s (TArrow l r) = TArrow (apply s l) (apply s r)
>    apply s t            = t

>    fv (TyVar v)    = Set.singleton v
>    fv (TArrow l r) = fv l `Set.union` fv r
>    fv _            = Set.empty

> instance Substitutable Sigma where
>    apply s (Forall vs tau) = Forall vs (apply s' tau)
>                    where s' = foldr Map.delete s vs
>    fv (Forall vs tau) = fv tau `Set.difference` (Set.fromList vs)

Substitution composition
-------------------------

> compose :: Subst -> Subst -> Subst
> s `compose` s' = Map.map (apply s) s' `Map.union` s

