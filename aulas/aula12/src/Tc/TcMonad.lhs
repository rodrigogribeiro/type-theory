Type inference monad
====================

> module Tc.TcMonad where

> import Control.Monad.Except
> import Control.Monad.Identity
> import Control.Monad.State
> import Control.Monad.RWS 

> import Data.List(intersperse)
> import Data.Map (Map)
> import qualified Data.Map as Map

> import Text.PrettyPrint.HughesPJ

> import Data.Syntax

> import Utils.PPrint


Typing contexts
---------------

> newtype Ctx = Ctx { unCtx :: Map Name Sigma }
>               deriving (Eq , Ord)

> instance PPrint Ctx where
>    pprint =  hcat . intersperse nl . map pprSignature . Map.toList . unCtx

> pprSignature :: (Name , Sigma) -> Doc
> pprSignature (n, (Forall vs t))
>              | null vs   = pprint t
>              | otherwise = hsep [lforall ,
>                                  pprint vs ,
>                                  pprint t]


Constraints are just a pair of types
------------------------------------

> newtype Constraint = Constraint{ unConstr :: (Tau, Tau) }
>                      deriving (Eq, Ord)

> instance PPrint Constraint where
>    pprint (Constraint (t,t')) = hsep [pprint t, equals,
>                                       pprint t']

A monad for collecting constraints and generating fresh variables
-----------------------------------------------------------------

> type TcM a = (RWST Ctx [Constraint] Int (ExceptT String Identity)) a

> addConstr :: Tau -> Tau -> TcM ()
> addConstr tau tau' = tell [ Constraint (tau, tau') ] 

> fresh :: TcM Tau
> fresh = do
>           n <- get
>           put (n + 1)
>           let nm = "x_" ++ show n
>           return (TyVar (Tyvar (Name nm)))