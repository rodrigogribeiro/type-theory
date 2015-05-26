Semantic Analysis Monad
=================

Definition of a simple monad for type and kind inference.

> module Utils.SemanticMonad where

> import Control.Monad.Except
> import Control.Monad.Identity
> import Control.Monad.State
> import Control.Monad.RWS 

> import Data.Map (Map)
> import qualified Data.Map as Map

> import Text.PrettyPrint.HughesPJ

> import Data.Syntax

> import Utils.PPrint


Contexts
---------

> newtype Ctx a = Ctx { unCtx :: Map Name a }
>               deriving (Eq , Ord)

> emptyCtx :: Ctx
> emptyCtx = Ctx Map.empty

> instance PPrint a => PPrint (Ctx a) where
>    pprint =  hcat . intersperse nl . map pprSignature . Map.toList . unCtx

> pprSignature :: PPrint a => (Name , a) -> Doc
> pprSignature (n, sig) = pprint n <+> text "::" <+> pprint sig

Monad definition
----------------

> type ScM a b c = (RWST (Ctx b) [c] Int (ExceptT String Identity)) a

> runScM :: Ctx b -> TcM a b c -> Either String (a , [c])
> runScM ctx m = runExcept (evalRWST m ctx 0)

> record :: c -> c -> ScM () b c
> record c c' = 


> freshName :: ScM Name b c
> freshName = do
>              n <- get
>              put (n + 1)
>              let nm = "x_" ++ show n
>              return (Name nm)

> insertEnv :: Name -> Sigma -> Ctx -> Ctx
> insertEnv n sig (Ctx m) = Ctx (Map.insert n sig m)


