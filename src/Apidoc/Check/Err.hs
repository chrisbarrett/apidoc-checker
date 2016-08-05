module Apidoc.Check.Err where

import           Apidoc.Pos
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data Err = Err Pos
  deriving (Show, Eq, Ord)

render :: Err -> PP.Doc
render = undefined
