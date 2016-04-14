{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
module Apidoc.Orphans where

import           Control.Applicative
import           Data.Validation     (Validation (..))

instance Monoid e => Alternative (Validation e) where
    empty = Failure mempty
    l@Success {} <|> _ = l
    Failure {}   <|> r = r
