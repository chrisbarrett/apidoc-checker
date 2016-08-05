module Apidoc.Pos where

data Pos = Pos {
      _posLine :: !Integer
    , _posCol  :: !Integer
    }
    deriving (Show, Eq, Ord)

empty :: Pos
empty = Pos 0 0
