module Apidoc.Check.Env where

import           Apidoc.Check.Err (Err)
import           Apidoc.DSL       (TypeName)
import           Data.Sequence    (Seq)

data Env = Env {
    _envTypes :: !(Seq TypeName)
  , _envErrs  :: !(Seq Err)
  }
  deriving (Show, Eq, Ord)

instance Monoid Env where
    mempty = Env mempty mempty
    mappend (Env x1 y1) (Env x2 y2) = Env (x1 `mappend` x2) (y1 `mappend` y2)
