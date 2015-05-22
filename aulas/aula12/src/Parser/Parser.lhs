Parser for coreML
===========

> module Parser.Parser where

> import Control.Monad.Except

> import Data.Either (either)
> import Data.Syntax hiding (term)

> import Parser.Lexer 

> import Text.Parsec hiding (bool)
> import qualified Text.Parsec.Token as Tok



Definitions and module
----------------------

> newtype Def = Def { unDef :: (Name, Term) }
>               deriving (Eq, Ord)

> def = do
>         n <- variable
>         reservedOp "="
>         t <- term
>         return (Def (n,t))

> top = do
>         d <- def
>         optional semi
>         return d

> modl = many1 top


Term syntax
-----------


> lam = do
>         reservedOp "\\"
>         vs <- many1 variable
>         reservedOp "->"
>         t <- term
>         return (foldr Abs t vs)

> binding = do
>           v <- variable
>           reservedOp "="
>           t <- term
>           return (Bind v t)

> letin = do
>           reserved "let"
>           b <- binding
>           reserved "in"
>           t' <- term
>           return (Let b t')


> base = liftM Var variable <|>
>        liftM Const lit    <|>
>        lam                <|>
>        letin


> term = return . foldl1 App =<< many1 base

Identifiers and other tokens
----------------------------


> variable = return . Name =<< identifier


Parsing literals
----------------


> lit = (charLit >>= return . CLit) <|>
>       (int     >>= return . ILit) <|>
>       (bool    >>= return . BLit)


> int = return . fromInteger =<< Tok.integer lexer


> bool = (reserved "True"  >> return True) <|>
>        (reserved "False" >> return False)