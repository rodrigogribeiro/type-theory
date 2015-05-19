Interpreter for Core-ML
===============

> module Eval.Eval where

> import Control.Monad.Identity

> import Data.Map(Map)
> import qualified Data.Map as Map

> import Data.Syntax

Term environment
--------------

> type TermEnv = Map Name Value
> type Interpreter t = Identity t

Value definition
----------------

> data Value = VInt Int
>            | VBool Bool
>            | VChar Char
>            | VClosure Name Term TermEnv

Interpreter code
----------------

> eval :: TermEnv -> Term -> Interpreter Value
> eval env (Const c) = evalConst c
> eval env (Var v)
>      = do
>         let Just v' = Map.lookup v env
>         return v'
> eval env (Abs v t)
>      = return (VClosure v t env)
> eval env (App l r)
>      = do
>          (VClosure n fun e') <- eval env l
>          arg <- eval env r
>          let nenv = Map.insert n arg env
>          eval nenv fun
> eval env (Let n e e')
>      = do
>          v <- eval env e
>          let nenv = Map.insert n v env
>          eval nenv e'

> evalConst :: Lit -> Interpreter Value
> evalConst (ILit i) = return (VInt i)
> evalConst (BLit b) = return (VBool b)
> evalConst (CLit c) = return (VChar c)

> runEval :: TermEnv -> Name -> Term -> (Value, TermEnv)
> runEval env nm ex =
>         let res = runIdentity (eval env ex) in
>         (res, Map.insert nm res env)

Auxiliar code
-------------

> instance Show Value where
>    show (VInt i) = show i
>    show (VBool b) = show b
>    show (VChar c) = show c
>    show (VClosure _ _ _) = "<<closure>>"
