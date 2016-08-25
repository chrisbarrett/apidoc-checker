{-# LANGUAGE TemplateHaskell #-}
module Apidoc.Pos where

import           Control.Lens.TH
import           Text.Trifecta.Delta (Delta (..))

newtype Pos = Pos {_unPos :: Delta}
    deriving (Ord, Show)

instance Eq Pos where
    _ == _ = True


empty :: Pos
empty = Pos (Columns (-1) (-1))

makeLenses ''Pos
makePrisms ''Pos
