{-# LANGUAGE TemplateHaskell #-}
module Apidoc.Check.Lenses where

import           Apidoc.Check.Env
import           Apidoc.Check.Err
import           Control.Lens.TH

makePrisms ''Env
makeLenses ''Env

makePrisms ''Expected
makeLenses ''Expected

makePrisms ''Actual
makeLenses ''Actual

makePrisms ''Err
makeLenses ''Err

makePrisms ''ErrType
makeLenses ''ErrType
