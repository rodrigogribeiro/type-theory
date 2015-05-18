Syntax definition for Core ML
===================

Here, in this module I define a syntax definition for CoreML.

> module Data.Syntax where

> import Text.PrettyPrint.HughesPJ

A type for names
----------------

> newtype Name = Name { out :: String }
>                deriving (Eq, Ord)


Constants
---------

> data Lit = ILit Int | BLit Bool |
>            CLit Char
>            deriving (Eq, Ord)


Term syntax
-----------

> data Term = Var Name          |
>             Abs Name Term     |
>             App Term Term     |
>             Const Lit         |
>             Let Name Term Term
>             deriving (Eq, Ord)


Type syntax
-----------

> type Tyvar = Name

> data Tau = TyVar Tyvar    |
>            TInt           |
>            TBool          |
>            TChar          |
>            TArrow Tau Tau
>            deriving (Eq, Ord)

> data Sigma = Forall [Name] Tau
>              deriving (Eq, Ord)


Pretty-printing 
---------------

> class PPrint a where
>    pprint :: a -> Doc

> instance PPrint a => PPrint [a] where
>    pprint = hsep . map pprint

> instance PPrint Name where
>    pprint = text . out

> instance PPrint Lit where
>    pprint (ILit i) = int i
>    pprint (BLit b) = bool b
>    pprint (CLit c) = char c

> instance PPrint Term where
>    pprint (Var n) = pprint n
>    pprint (Abs n t) = hsep [slash , pprint n ,
>                             dot , pprint t]
>    pprint (Const c) = pprint c
>    pprint (Let n t t') = llet <> body <+> lin <+> pprint t'
>                          where
>                            body = braces (nest 3 (empty $+$ eqs))
>                            eqs = hsep [pprint n, equals , pprint t]

> instance PPrint Tau where
>    pprint (TyVar v) = pprint v
>    pprint TInt      = text "Int"
>    pprint TBool     = text "Bool"
>    pprint TChar     = text "Char"
>    pprint (TArrow t t')
>           | isArrow t = hsep [parens (pprint t) , arrow ,
>                               pprint t']
>           | otherwise = hsep [pprint t , arrow , pprint t']

> instance PPrint Sigma where
>    pprint (Forall vs tau)
>           | null vs = pprint tau
>           | otherwise = hsep [tforall , pprint vs , dot ,
>                               pprint tau]

Auxiliar code for pretty printing

> bool :: Bool -> Doc
> bool = text . show

> slash :: Doc
> slash = char '\\'

> dot :: Doc
> dot = char '.'

> llet :: Doc
> llet = text "let"

> lin :: Doc
> lin = text "in"

> isArrow :: Tau -> Bool
> isArrow (TArrow _ _) = True
> isArrow _            = False

> arrow :: Doc
> arrow = text "->"

> tforall :: Doc
> tforall = text "forall"
