> {-# LANGUAGE FlexibleContexts #-}

Simple type inference for Core-ML
=================================

Here, I show the implementation of a very simple type inference
algorithm for Core ML.

The shell doesn't work quite well, I'm tired of implementing this. :(

> module Main where

> import Control.Monad.State.Strict

> import Data.List
> import Data.Monoid
> import Data.Map(Map)
> import qualified Data.Map as Map

> import Data.Syntax

> import Eval.Eval

> import Parser.Parser (parser)

> import Tc.TcMonad
> import Tc.Tc

> import System.Environment
> import System.Exit
> import System.Console.Repline

Shell internal state
--------------------

> data ShellState = ShellState { typeEnv :: Ctx , termEnv :: TermEnv }

> initState :: ShellState
> initState = ShellState (Ctx Map.empty) Map.empty

Shell monad
------------

> type Repl a = HaskelineT (StateT ShellState IO) a

> hoistError :: PPrint a => Either String a -> Repl a
> hoistError (Left s)
>            = do
>                liftIO (print s)
>                abort
> hoistError (Right v) = return v

Shell itself
------------

> shell :: Repl a -> IO ()
> shell rep = flip evalStateT initState $
>                             evalRepl "coreML>" cmd
>                                      options completer rep

Interpreter definition
----------------------

> exec :: String -> Repl ()
> exec src
>      = do
>          let it = Name "it"
>          st <- get
>          mod <- hoistError (parser src)
>          t' <- hoistError (inference (typeEnv st) mod)
>          let (val,_) = runEval (termEnv st) it mod
>              m = pprint mod
>              m' = pprint t'
>          liftIO $ putStrLn $ (show m) ++ "::" ++ (show m')
>          liftIO $ putStrLn $ (show val)

> cmd :: String -> Repl ()
> cmd src = exec src


Interpreter commands
--------------------

> browse :: [String] -> Repl ()
> browse _
>      = do
>         st <- get
>         liftIO $ mapM_ (putStrLn . show . pprint) (Map.elems $ unCtx $ typeEnv st)

> load :: [String] -> Repl ()
> load args
>      = do
>          cont <- liftIO $ readFile (unwords args)
>          exec cont

> typeOf :: [String] -> Repl ()
> typeOf args
>      = do
>          st <- get
>          let arg = unwords args
>          case Map.lookup (Name arg) (unCtx $ typeEnv st) of
>               Just ty -> liftIO $ putStrLn $ arg ++ " :: " ++ (show $ pprint ty)
>               Nothing -> exec arg

> quit :: a -> Repl ()
> quit _ = liftIO $ exitSuccess

Tab completion
---------------


> defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
> defaultMatcher = [ (":load"  , fileCompleter) ]

> comp :: (Monad m, MonadState ShellState m) => WordCompleter m
> comp n = do
>            let cmds = [":load", ":browse", ":quit", ":type"]
>            ctx <- gets typeEnv
>            let defs = Map.keys $ unCtx ctx
>            return $ filter (isPrefixOf n) (cmds ++ map out defs)

> options :: [(String, [String] -> Repl ())]
> options = [
>    ("load"   , load)
>  , ("browse" , browse)
>  , ("quit"   , quit)
>  , ("type"   , Main.typeOf)
>  ]

> completer :: CompleterStyle (StateT ShellState IO)
> completer = Prefix (wordCompleter comp) defaultMatcher

Main function
-------------

> main :: IO ()
> main = do
>          args <- getArgs
>          case args of
>             []      -> shell (return ())
>             [fname] -> shell (load [fname])
>             _       -> putStrLn "Invalid arguments!"
