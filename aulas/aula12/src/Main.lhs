Constraint based type inference for Core-ML
============================

> module Main where

> import Data.List (intersperse)

> import Text.PrettyPrint.HughesPJ (hcat)

> import System.Environment (getArgs)

> import Data.Syntax

> import Parser.Parser (parseModule)

> import Tc.ConstraintGen
> import Tc.TcMonad

> import Utils.PPrint



> showOutput :: Name -> (Sigma, [Constraint]) -> IO ()
> showOutput n (sig, cs)
>              = putStrLn (show $ pprSignature (n, sig))

> inferBinds :: Ctx -> [Binding] -> IO ()
> inferBinds ctx [] = return ()
> inferBinds ctx (b : bs)
>                   = do
>                       either putStrLn
>                              (showOutput (name b))
>                              (infer ctx (term b))
>                       inferBinds ctx bs


> typeInferCoreML :: String -> IO ()
> typeInferCoreML fname
>                 = do
>                      cont <- readFile fname
>                      let t = parseModule cont
>                      case t of
>                          Left err -> putStrLn err
>                          Right bs -> inferBinds emptyCtx bs


> main :: IO ()
> main = do
>          args <- getArgs
>          case args of
>             [fname] -> typeInferCoreML fname
>             _       -> putStrLn "Invalid parameters!"