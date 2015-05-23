Parser for coreML
===========

> module Parser.Parser where

> import Control.Monad.Except

> import Data.Either (either)
> import Data.Syntax hiding (term)

> import Parser.Lexer 

> import Text.Parsec hiding (bool)
> import qualified Text.Parsec.Token as Tok


Top level parsing function
--------------------------

> parseModule :: String -> Either String [Binding]
> parseModule cont
>             = case parse modl "" cont of
>                    Left err -> Left $ show err
>                    Right bs -> Right bs


Definitions and module
----------------------


> top = do
>         d <- binding
>         semi
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
>        letin              <|>
>        parens term


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