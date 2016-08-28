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
        in Trifecta.explain r (toTrifectaErr e) <> PP.line

    toTrifectaErr e = mempty & Trifecta.reason .~ Just (note e)

    text = PP.text . Text.unpack

    note err = case err ^. errType of

        TypeError (Expected e) (Actual a) ->
            "Type error. Expected: ‘" <> text e <> "’, but got: ‘" <> text a <> "’."

        RequiredKeyMissing k ->
            "Missing required key: ‘" <> text k <> "’."

        UnexpectedKey k ->
            "Unexpected key: ‘" <> text k <> "’."

        InvalidUri _ ->
            "Invalid URI."

        InvalidHttpMethod m ->
            "Invalid HTTP method: ‘" <> text m <> "’."

        InvalidParameterLocation loc ->
            "Invalid parameter location: ‘" <> text loc <> "’."

        ResponseCodeOutOfRange n ->
            "Response code out of range: ‘" <> PP.text (show n) <> "’."

        UnparseableResponseCode s ->
            "Invalid response code: ‘" <> text s <> "’."

        UnparseableTypeRef t ->
            "Invalid type: ‘" <> text t <> "’."

        DuplicateKey _ dup ->
            "Duplicated key: ‘" <> text (dup ^. keyLabel) <> "’."
