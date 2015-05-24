Syntax definition for Core-ML 
=============================

> module Data.Syntax where

> import Data.Char (chr, ord)

> import Text.PrettyPrint.HughesPJ

> import Utils.PPrint


Name definition
---------------

> newtype Name = Name{ unName :: String }
>                deriving (Eq, Ord)


Binding definition
------------------

> data Binding = Bind { name :: Name
>                     , term :: Term }
>                deriving (Eq, Ord)


Literals
--------

> data Lit = ILit Int | CLit Char
>            deriving (Eq, Ord)


Term definition
---------------

> data Term = Const Lit
>           | Var Name
>           | Con Name
>           | Abs Name Term
>           | App Term Term
>           | Let Binding Term
>           deriving (Eq, Ord)


Type definition
----------------

> newtype Tyvar = Tyvar { unTyvar :: Name }
>                 deriving (Eq , Ord)

> newtype Tycon = Tycon {unTycon :: Name }
>                 deriving (Eq, Ord)

Unquantified types

> data Tau = TyVar Tyvar
>          | TyCon Tycon
>          | TApp Tau Tau
>          | TArrow Tau Tau
>          deriving (Eq, Ord)

types schemes

> data Sigma = Forall [Tyvar] Tau
>              deriving (Eq, Ord)


Kind definition
---------------

> data Kind = Star | KArrow Kind Kind
>             deriving (Eq, Ord)


Built-in stuff
--------------

> tArrow :: Tau
> tArrow = TyCon (Tycon (Name "->"))

> infixr 4 +->

> (+->) :: Tau -> Tau -> Tau
> l +-> r = TApp (TApp tArrow l) r

> tInt :: Tau
> tInt = TyCon (Tycon (Name "Int"))

> tChar :: Tau
> tChar = TyCon (Tycon (Name "Char"))


Pretty printting stuff
----------------------

> instance PPrint Name where
>    pprint = text . unName

> instance PPrint Lit where
>    pprint (ILit i) = int i
>    pprint (CLit c) = char c

> instance PPrint Binding where
>    pprint (Bind n t) = hsep [pprint n , equals , pprint t]

> instance PPrint Term where
>    pprint (Var v) = pprint v
>    pprint (Con c) = pprint c
>    pprint (Const l) = pprint l
>    pprint (Abs v t) = hsep [lam , pprint v ,
>                             arrow , pprint t]
>    pprint (App l r) = parensIf (isApp l) l <+> pprint r
>    pprint (Let bnd t) = hsep [llet , pprint bnd , lin , pprint t]

> instance PPrint Tyvar where
>    pprint = pprint . unTyvar

> instance PPrint Tycon where
>    pprint = pprint . unTycon

> instance PPrint Tau where
>    pprint (TyVar n) = pprint n
>    pprint (TyCon n) = pprint n
>    pprint (TApp l r) = parensIf (isTApp l) l <+> pprint r
>    pprint (TArrow l r) = hsep [parensIf (isArrow l) l,
>                                arrow ,
>                                pprint r]


> instance PPrint Sigma where
>    pprint (Forall vs tau)
>           | null vs   = pprint tau
>           | otherwise = hsep [lforall ,
>                               pprint vs , dot ,
>                               pprint tau]

> instance PPrint Kind where
>    pprint Star = char '*'
>    pprint (KArrow l r) = hsep [parensIf (isKArrow l) l,
>                                arrow,
>                                pprint r]

Auxiliar functions
------------------

> isApp :: Term -> Bool
> isApp (App _ _) = True
> isApp _         = False

> isArrow :: Tau -> Bool
> isArrow (TArrow _ _) = True
> isArrow _            = False

> isTApp :: Tau -> Bool
> isTApp (TApp _ _) = True
> isTApp _          = False

> isKArrow :: Kind -> Bool
> isKArrow (KArrow _ _) = True
> isKArrow _            = False