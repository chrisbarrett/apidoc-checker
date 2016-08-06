module Apidoc.Check.Err where

import qualified Apidoc.Json                  as Json
import           Apidoc.Pos
import           Data.Text                    (Text)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

newtype Expected = Expected {_expectedLabel :: Text}
  deriving (Show, Eq, Ord)

newtype Actual = Actual {_actualLabel :: Text}
  deriving (Show, Eq, Ord)

data Err = Err Pos ErrType
  deriving (Show, Eq, Ord)

data ErrType = TypeError Expected Actual
             | RequiredKeyMissing Text
             | UnexpectedKey Text
             | InvalidUri Text
             | DuplicateKey
                  Json.Key -- ^ First declaration
                  Json.Key -- ^ Duplicate declaration

  deriving (Show, Eq, Ord)

render :: Err -> PP.Doc
render = undefined
