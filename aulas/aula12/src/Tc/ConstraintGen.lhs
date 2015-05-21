Constraint generation for coreML
=====================

> module Tc.ConstraintGen where

> import Control.Monad.Identity
> import Control.Monad.Reader
> import Control.Monad.State
> import Control.Monad.Writer

> import Data.List (intersperse)
> import Data.Map(Map)
> import qualified Data.Map as Map

> import Data.Syntax

> import Tc.TcMonad

> import Utils.PPrint 



