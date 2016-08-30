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

    quoted s = "‘" <> s <> "’"

    note err = case err ^. errType of

        TypeError (Expected e) (Actual a) ->
            "Type error. Expected: " <> text (quoted e) <> ", but got: " <> text (quoted a) <> "."

        RequiredKeyMissing k ->
            "Missing required key: " <> text (quoted k) <> "."

        UnexpectedKey k Nothing ->
            "Unexpected key: " <> text (quoted k) <> "."

        UnexpectedKey k (Just s) ->
            "Unexpected key: " <> text (quoted k) <> ". Did you mean " <> text (quoted s) <> "?"

        InvalidUri {} ->
            "Invalid URI."

        InvalidHttpMethod m ->
            "Invalid HTTP method: " <> text (quoted m) <> "."

        InvalidParameterLocation loc ->
            "Invalid parameter location: " <> text (quoted loc) <> "."

        ResponseCodeOutOfRange n ->
            "Response code out of range: " <> PP.text (quoted (show n)) <> "."

        UnparseableResponseCode s ->
            "Invalid response code: " <> text (quoted s) <> "."

        UnparseableTypeRef t ->
            "Invalid type name: " <> text (quoted t) <> "."

        DuplicateKey _ dup ->
            "Duplicated key: " <> text (quoted (dup ^. keyLabel)) <> "."
