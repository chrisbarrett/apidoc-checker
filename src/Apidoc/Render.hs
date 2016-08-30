{-# LANGUAGE OverloadedStrings #-}
module Apidoc.Render where

import           Apidoc.Check.Err
import           Apidoc.Check.Lenses
import           Apidoc.Json.Lenses
import           Apidoc.Pos
import           Control.Lens
import qualified Data.Foldable                as Foldable
import           Data.Monoid                  ((<>))
import qualified Data.Text                    as Text
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Text.Trifecta                as Trifecta

renderErrs :: Traversable f => f Err -> PP.Doc
renderErrs es =
    PP.vcat (renderErr <$> Foldable.toList es)
  where
    renderErr :: Err -> PP.Doc
    renderErr e =
        -- TODO: Show error context once I figure out how to render that properly.
        let r = mempty & Trifecta.renderingDelta .~ (e ^. errPos._Pos)
        in Trifecta.explain r (toTrifectaErr e)

    toTrifectaErr e = mempty & Trifecta.reason .~ Just (note e)

    text = PP.text . Text.unpack

    quoted s = text ("‘" <> s <> "’")
    quoted' s = PP.text ("‘" <> s <> "’")

    note err = case err ^. errType of

        TypeError (Expected e) (Actual a) ->
            "Type error. Expected " <> quoted e <> ", but got " <> quoted a <> "."

        RequiredKeyMissing k ->
            "Missing required key: " <> quoted k <> "."

        UnexpectedKey k Nothing ->
            "Unexpected key: " <> quoted k <> "."

        UnexpectedKey k (Just s) ->
            "Unexpected key: " <> quoted k <> ". Did you mean " <> quoted s <> "?"

        InvalidUri {} ->
            "Invalid URI."

        InvalidHttpMethod m (Just s) ->
            "Invalid HTTP method: " <> quoted m <> ". Did you mean " <> quoted s <> "?"

        InvalidHttpMethod m Nothing ->
            "Invalid HTTP method: " <> quoted m <> ". "
            <> "Expected a standard HTTP method like " <> quoted "GET" <> ", " <> quoted "PUT"
            <> quoted "POST" <> ", " <> quoted "DELETE" <> ", etc."

        InvalidParameterLocation loc (Just s) ->
            "Invalid parameter location: " <> quoted loc <> ". Did you mean " <> quoted s <> "?"

        InvalidParameterLocation loc Nothing ->
            "Invalid parameter location: " <> quoted loc <> ". "
            <> "Expected " <> quoted "path" <> ", " <> quoted "query" <> " or " <> quoted "form" <> "."

        ResponseCodeOutOfRange n ->
            "Response code out of range: " <> quoted' (show n) <> ". Expected a number between 200 and 526."

        UnparseableResponseCode s ->
            "Invalid response code: " <> quoted s <> ". Expected a number between 200 and 526."

        UnparseableTypeRef t ->
            "Invalid type: " <> quoted t <> "."

        DuplicateKey _ dup ->
            "Duplicated key: " <> quoted (dup ^. keyLabel) <> ". A key should occur at most once."
