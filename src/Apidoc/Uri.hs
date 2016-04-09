module Apidoc.Uri where

import qualified Data.Aeson       as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Maybe       (fromMaybe)
import qualified Data.Text        as Text
import           Network.URI      (URI, parseURI)

newtype Uri = Uri URI
  deriving (Show, Eq)

instance Aeson.FromJSON Uri where
    parseJSON (Aeson.String s) =
        fromMaybe (fail "Invalid URI")
                  (pure . Uri <$> parseURI (Text.unpack s))

    parseJSON invalid = Aeson.typeMismatch "Uri" invalid
