Type inference monad definition
-------------------------

> module Tc.TcMonad where

> import Control.Monad.Except
> import Control.Monad.Identity
> import Control.Monad.Reader
> import Control.Monad.State

> import Data.Map(Map)
> import qualified Data.Map as Map

> import Data.Syntax

A type context is just a mapping between variables and types

> newtype Ctx = Ctx{ unCtx :: Map Name Sigma }

Definition of the type inference monad

> type TcM a =  ReaderT Ctx (ExceptT String (StateT Int Identity)) a

> runTcM :: Ctx -> TcM a -> (Either String a, Int)
> runTcM ctx tcm = runIdentity (runStateT (runExceptT (runReaderT tcm ctx)) 0)

Creating a fresh type variable

> fresh :: TcM Tau
> fresh
>   = do
>       v <- get
>       put (v + 1)
>       return (TyVar (Name (letters !! v)))

> letters :: [String]
> letters = [1 .. ] >>= flip replicateM ['a' .. 'z']

Looking up a name in enviroment

> lookupEnv :: Name -> TcM Sigma
> lookupEnv n
>   = maybe (variableNotFound n)
>           return
>           =<< asks (Map.lookup n . unCtx)


Adding a name to a environment

> insertEnv :: Name -> Sigma -> Ctx -> Ctx
> insertEnv n s = Ctx . Map.insert n s . unCtx

Auxiliar functions

> variableNotFound :: Name -> TcM a
> variableNotFound n = throwError msg
>      where msg = "Undefined variable:" ++ (show (pprint n))
