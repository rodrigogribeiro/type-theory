Simple type inference for Core-ML
=================================

Here, I show the implementation of a very simple type inference
algorithm for Core ML.

> module Main where

> import Control.Monad.State

> import Data.List
> import Data.Monoid
> import Data.Map(Map)
> import qualified Data.Map as Map

> import Data.Syntax

> import Eval.Eval

> import Tc.TcMonad
> import Tc.Tc

> import System.Environment
> import System.Exit
> import System.Console.Repline

Shell internal state
--------------------

> data ShellState = ShellState { typeEnv :: Ctx , termEnv :: TermEnv }

> initState :: ShellState
> initState = ShellState Map.empty

Shell monad
------------

> type Repl a = HaskelineT (StateT ShellState IO) a

> hoistError :: Show a => Either String a -> Repl a
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

> exec :: Bool -> String -> Repl ()
> exec upd src
>      = do
>          st <- get
>          mod <- hoistError $ parser  src
>          tyctx' <- hoistError $ inference (typeEnv st) mod
>          let st' = st {
>                         termEnv = foldl' evalDef (termEnv st) mod
>                       , typeEnv = tyctx' <> typeEnv st
>                       }
>          when upd (put st')
>          case lookup "it" mod of
>               Nothing -> return ()
>               Just v  -> do
>                            let (val,_) = runEval (termEnv st') "it" v
>                            showOutput (show val) st'

> showOutput :: String -> ShellState -> Repl ()
> showOutput s st
>            = do
>                case Map.lookup (Name s) (typeEnv st) of
>                     Just ty -> liftIO $ putStrLn $ s ++ "::" ++ pprint ty
>                     Nothing -> return ()

> evalDef :: TermEnv -> (Name, Term) -> TermEnv
> evalDef env (n,t) = snd (runEval env n t)

> cmd :: String -> Repl ()
> cmd src = exec True src


Interpreter commands
--------------------

> browse :: [String] -> Repl ()
> browse _
>      = do
>         st <- get
>         liftIO $ mapM_ putStrLn $ pprint (typeEnv st)

> load :: [String] -> Repl ()
> load args
>      = do
>          cont <- liftIO $ readFile (unwords args)
>          exec True cont

> typeOf :: [String] -> Repl ()
> typeOf args
>      = do
>          st <- get
>          let arg = unwords args
>          case Map.lookup (Name arg) (typeEnv st) of
>               Just ty -> liftIO $ putStrLn $ arg ++ "::" ++ pprint ty
>               Nothing -> exec False arg

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
>            let defs = Map.keys ctx
>            return $ filter (isPrefixOf n) (cmds ++ defs)

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
> main
>    = do
>        args <- getArgs
>        case args of
>             []      -> shell (return ())
>             [fname] -> shell (load [fname])
>             _       -> putStrLn "Invalid arguments!"

