{-# LANGUAGE OverloadedStrings #-}
-- |Implements a JSON parser that approximately conforms to the JSON spec.
module Apidoc.Json (module Exports, pprint) where

import           Apidoc.Json.Lenses as Exports
import           Apidoc.Json.Parser as Exports
import           Apidoc.Json.Types  as Exports
import           Control.Lens
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import qualified Data.Text          as Text

pprint :: Json -> Text
pprint (JNumber _ n)   = Text.pack (show n)
pprint (JString _ s)   = Text.pack (show s)
pprint (JBool _ True)  = "true"
pprint (JBool _ False) = "false"
pprint (JNull _)       = "null"

pprint (JArray _ xs) =
    "[" <> commaSep values <> "]"
  where
    values = xs ^.. traverse.to pprint

pprint (JObject _ o) =
    "{" <> commaSep kvps <> "}"
  where
    kvps = o ^.. objectContent.traverse.to pprintKvp
    pprintKvp (k, v) = (k ^. keyLabel) <> ": " <> pprint v


commaSep :: [Text] -> Text
commaSep = Text.intercalate ","
