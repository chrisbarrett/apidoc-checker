{-# LANGUAGE OverloadedStrings #-}
module Apidoc.Render where

import           Apidoc.Check.Err
import           Apidoc.Check.Lenses
import           Apidoc.Json.Lenses
import           Apidoc.Pos
import           Control.Lens
import           Data.ByteString              (ByteString)
import           Data.Foldable                as Foldable
import           Data.Monoid                  ((<>))
import qualified Data.Text                    as Text
import           Debug.Trace
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Text.Trifecta                as Trifecta

renderErrs :: (Traversable f, Foldable f) => ByteString -> f Err -> PP.Doc
renderErrs bs es =
    trace ("Render errors: " <> show (es ^.. traverse.errPos._Pos)) $ do
    PP.vcat (renderErr <$> Foldable.toList es)
  where
    renderErr :: Err -> PP.Doc
    renderErr e =
        let r = Trifecta.renderingCaret (e ^. errPos._Pos) bs
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
