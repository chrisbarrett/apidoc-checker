{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Apidoc.Check.CheckM where

import           Apidoc.Check.Env
import           Control.Monad
import           Control.Monad.State (MonadState, StateT)
import qualified Control.Monad.State as State

newtype CheckM m a = CheckM (StateT Env m a)
    deriving (Functor, Applicative, Monad, MonadState Env)

type Check m = (Functor m, Applicative m, Monad m, MonadState Env m, MonadPlus m)

runCheckM :: MonadPlus m => CheckM m a -> Env -> m (a, Env)
runCheckM (CheckM m) e =
    State.runStateT m e
