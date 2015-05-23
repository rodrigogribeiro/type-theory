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

> data Lit = ILit Int | BLit Bool | CLit Char
>            deriving (Eq, Ord)


Term definition
---------------

> data Term = Const Lit
>           | Var Name
>           | Abs Name Term
>           | App Term Term
>           | Let Binding Term
>           deriving (Eq, Ord)


Type definition
----------------

> newtype Tyvar = Tyvar { unTyvar :: Name }
>                 deriving (Eq , Ord)

Unquantified types

> data Tau = TyVar Tyvar
>          | TInt
>          | TBool
>          | TChar
>          | TArrow Tau Tau
>          deriving (Eq, Ord)

types schemes

> data Sigma = Forall [Tyvar] Tau
>              deriving (Eq, Ord)


Pretty printting stuff
----------------------

> instance PPrint Name where
>    pprint = text . unName

> instance PPrint Lit where
>    pprint (ILit i) = int i
>    pprint (BLit b) = bool b
>    pprint (CLit c) = char c

> instance PPrint Binding where
>    pprint (Bind n t) = hsep [pprint n , equals , pprint t]

> instance PPrint Term where
>    pprint (Var v) = pprint v
>    pprint (Const l) = pprint l
>    pprint (Abs v t) = hsep [lam , pprint v ,
>                             arrow , pprint t]
>    pprint (App l r) = parensIf (isApp l) l <+> pprint r
>    pprint (Let bnd t) = hsep [llet , pprint bnd , lin , pprint t]

> instance PPrint Tyvar where
>    pprint = pprint . unTyvar

> instance PPrint Tau where
>    pprint (TyVar n) = pprint n
>    pprint (TArrow l r) = parensIf (isArrow l) l <+> arrow <+> pprint r
>    pprint TInt = text "Int"
>    pprint TBool = text "Bool"
>    pprint TChar = text "Char"

> instance PPrint Sigma where
>    pprint (Forall vs tau)
>           | null vs   = pprint tau
>           | otherwise = hsep [lforall ,
>                               pprint vs , dot ,
>                               pprint tau]

Auxiliar functions
------------------

> isApp :: Term -> Bool
> isApp (App _ _) = True
> isApp _         = False

> isArrow :: Tau -> Bool
> isArrow (TArrow _ _) = True
> isArrow _            = False
