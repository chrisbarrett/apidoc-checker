{-# LANGUAGE TemplateHaskell #-}
module Apidoc.Json.Lenses where

import           Apidoc.Json.Types
import           Control.Lens.TH

makeLenses ''Json
makePrisms ''Json

makeLenses ''Object
makePrisms ''Object

makeLenses ''Key
makePrisms ''Key
