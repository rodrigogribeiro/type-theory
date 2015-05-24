Pretty printting
==========

> module Utils.PPrint where

> import Text.PrettyPrint.HughesPJ

A class definition for pretty printting
---------------------------------------

> class PPrint a where
>    pprint :: a -> Doc

> instance PPrint a => PPrint [a] where
>    pprint = hsep . map pprint

> ppr :: PPrint a => a -> String
> ppr = show . pprint

> parensIf :: PPrint a => Bool -> a -> Doc
> parensIf b e = if b then parens (pprint e) else pprint e

Some simple documents
---------------------

> lam :: Doc
> lam = char '\\'

> arrow :: Doc
> arrow = text "->"

> llet :: Doc
> llet = text "let"

> lin :: Doc
> lin = text "in"

> lforall :: Doc
> lforall = text "forall"

> nl :: Doc
> nl = char '\n'

> bool :: Bool -> Doc
> bool = text . show

> dot :: Doc
> dot = text "."