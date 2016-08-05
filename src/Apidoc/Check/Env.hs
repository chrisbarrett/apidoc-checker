module Apidoc.Check.Env where

import           Apidoc.DSL    (TypeName)
import           Data.Sequence (Seq)

data Env = Env {
    _envTypes :: Seq TypeName
  }
  deriving (Show, Eq, Ord)

instance Monoid Env where
    mempty = Env mempty
    mappend (Env t1) (Env t2) = Env (t1 `mappend` t2)
