Parser for Core-ML
============ 

> module Parser.Parser where

> import Control.Monad.Except

> import Data.Either (either)
> import Data.Syntax hiding (bool)

> import Parser.Lexer 

> import Text.Parsec hiding (bool)
> import qualified Text.Parsec.Token as Tok

Top level parser function
-------------------------

> parser :: String ->  Either String Term
> parser s = case (parse term "" s) of
>                 Left err -> Left $ show err
>                 Right t  -> Right t

> parserModule :: String -> Either String [(Name , Term)]
> parserModule cont = case (parse modl "" cont) of
>                          Left err -> Left $ show err
>                          Right t  -> Right (map unDef t)

Definitions and module
----------------------

> newtype Def = Def { unDef :: (Name, Term) }
>               deriving (Eq, Ord, Show)

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


> letin = do
>           reserved "let"
>           v <- variable
>           reservedOp "="
>           t <- term
>           reserved "in"
>           t' <- term
>           return (Let v t t')


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