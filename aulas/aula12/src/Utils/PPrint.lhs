Pretty printting
==========

> module Utils.PPrint where

A class definition for pretty printting
---------------------------------------

> class PPrint a where
>    pprint :: a -> Doc

> instance PPrint a => PPrint [a]
>    pprint = hsep . map pprint

> ppr :: PPrint a => a -> String
> ppr = show . pprint

> parensIf :: PPrint a => Bool -> a -> Doc
> parensIf b e = if b then parens else id $ pprint e

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
