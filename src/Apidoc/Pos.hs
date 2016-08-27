{-# LANGUAGE TemplateHaskell #-}
module Apidoc.Pos where

import           Control.Lens.TH
import           Text.Trifecta.Delta (Delta (..))

newtype Pos = Pos {_unPos :: Delta}
    deriving (Ord)

instance Eq Pos where
    _ == _ = True

instance Show Pos where
    show _ = "<pos>"


empty :: Pos
empty = Pos (Columns (-1) (-1))

makeLenses ''Pos
makePrisms ''Pos
