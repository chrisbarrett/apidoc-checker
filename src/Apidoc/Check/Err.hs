module Apidoc.Check.Err where

import qualified Apidoc.Json as Json
import           Apidoc.Pos
import           Data.Text   (Text)

newtype Expected = Expected {_expectedLabel :: Text}
  deriving (Show, Eq, Ord)

newtype Actual = Actual {_actualLabel :: Text}
  deriving (Show, Eq, Ord)

data ErrType = TypeError Expected Actual
             | RequiredKeyMissing Text
             | UnexpectedKey
                  Text         -- Found key
                  (Maybe Text) -- Suggestion
             | InvalidUri Text
             | InvalidHttpMethod
                   Text         -- Actual method
                   (Maybe Text) -- Suggestion
             | InvalidParameterLocation
                   Text         -- Actual location
                   (Maybe Text) -- Suggestion
             | ResponseCodeOutOfRange Integer
             | UnparseableResponseCode Text
             | UnparseableTypeRef Text
             | DuplicateKey
                  Json.Key -- First declaration
                  Json.Key -- Duplicate declaration
  deriving (Show, Eq, Ord)

data Err = Err {_errPos :: Pos, _errType :: ErrType}
  deriving (Show, Eq, Ord)
