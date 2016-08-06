{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Apidoc.Check.CheckObject where

import           Apidoc.Json         (Object)
import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.State (MonadState, State, StateT (..))
import qualified Control.Monad.State as State
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Text           (Text)

data CheckState = CheckState {
    _checkStateExpectedKeys :: !(Set Text)
  , _checkStateObject       :: !Object
  }

makeLenses ''CheckState
makePrisms ''CheckState

emptyCheckState :: Object -> CheckState
emptyCheckState js = CheckState mempty js

newtype CheckObject m a = CheckObject (StateT CheckState m a)
    deriving (Functor, Applicative, Monad, MonadState CheckState)

runCheckObject :: Monad m => CheckObject m a -> Object -> m (a, Set Text)
runCheckObject (CheckObject m) js = do
    (a, s) <- runStateT m (emptyCheckState js)
    pure (a , s ^. checkStateExpectedKeys)

askObject :: Monad m => CheckObject m Object
askObject = do
    s <- State.get
    CheckObject (pure (s ^. checkStateObject))

pushExpectedKey :: Monad m => Text -> CheckObject m ()
pushExpectedKey k = do
    checkStateExpectedKeys <%= (Set.insert k)
    CheckObject (pure ())
