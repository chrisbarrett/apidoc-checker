module Apidoc.Json.Types where

import           Apidoc.Pos
import           Data.Sequence (Seq)
import           Data.Text     (Text)

data Json = JObject Pos Object
          | JArray  Pos (Seq Json)
          | JNumber Pos Double
          | JString Pos Text
          | JBool   Pos Bool
          | JNull   Pos
  deriving (Eq, Show, Ord)

data Object = Object {
    _objectPos     :: !Pos
  , _objectContent :: !(Seq (Key, Json))
  } deriving (Eq, Show, Ord)

data Key = Key {
    _keyPos   :: !Pos
  , _keyLabel :: !Text
  } deriving (Eq, Ord)

instance Show Key where
    show (Key _ l) = show l
