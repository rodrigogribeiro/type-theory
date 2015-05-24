Lexer for coreML
===========

> module Parser.Lexer where

> import Text.Parsec
> import qualified Text.Parsec.Token as Tok
> import qualified Text.Parsec.Expr as Exp

> import Data.Functor.Identity

> type Parser a = ParsecT String () Identity a

> reservedNames :: [String]
> reservedNames = ["let", "in"]

> reservedOps :: [String]
> reservedOps = [ "->", "\\", "=" ]

> lexer :: Tok.GenTokenParser String () Identity
> lexer = Tok.makeTokenParser $ Tok.LanguageDef
>  { Tok.commentStart    = "{-"
>  , Tok.commentEnd      = "-}"
>  , Tok.commentLine     = "--"
>  , Tok.nestedComments  = True
>  , Tok.identStart      = letter
>  , Tok.identLetter     = alphaNum <|> oneOf "_'"
>  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
>  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
>  , Tok.reservedNames   = reservedNames
>  , Tok.reservedOpNames = reservedOps
>  , Tok.caseSensitive   = True
>  }

> reserved :: String -> Parser ()
> reserved = Tok.reserved lexer

> reservedOp :: String -> Parser ()
> reservedOp = Tok.reservedOp lexer

> identifier :: Parser String 
> identifier = Tok.identifier lexer

> parens :: Parser a -> Parser a
> parens = Tok.parens lexer

> semiSep :: Parser a -> Parser [a]
> semiSep = Tok.semiSep lexer

> semi :: Parser String
> semi = Tok.semi lexer

> charLit :: Parser Char
> charLit = Tok.charLiteral lexer

> contents :: Parser a -> Parser a
> contents p = do
>  Tok.whiteSpace lexer
>  r <- p
>  eof
>  return r