{-# LANGUAGE OverloadedStrings #-}
module Apidoc.Render where

import           Apidoc.Check.Err
import qualified Apidoc.Check.Err             as Check
import           Apidoc.Check.Lenses
import           Apidoc.Json.Lenses
import           Control.Lens
import           Data.ByteString              (ByteString)
import           Data.Monoid                  ((<>))
import           Data.Sequence                (Seq)
import qualified Data.Text                    as Text
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Text.Trifecta                as Trifecta

renderErrs :: ByteString -> Seq Check.Err -> PP.Doc
renderErrs bs es =
    case Trifecta.parseByteString raiseErrs mempty bs of
      Trifecta.Success _ -> mempty
      Trifecta.Failure doc -> doc
  where
    raiseErrs =
        traverse raiseErrAsMessage es

    raiseErrAsMessage err =
      Trifecta.raiseErr . Trifecta.failed . Text.unpack $
          case err ^. errType of
            TypeError (Expected e) (Actual a) ->
                "Type error. Expected: `" <> e <> "`, but got: `" <> a <> "`."

            RequiredKeyMissing k ->
                "Missing required key: `" <> k <> "`."

            UnexpectedKey k ->
                "Unexpected key: `" <> k <> "`."

            InvalidUri _ ->
                "Invalid URI."

            InvalidHttpMethod m ->
                "Invalid HTTP method: `" <> m <> "`."

            InvalidParameterLocation loc ->
                "Invalid parameter location: `" <> loc <> "`."

            ResponseCodeOutOfRange n ->
                "Response code out of range: `" <> Text.pack (show n) <> "`."

            UnparseableResponseCode s ->
                "Invalid response code: `" <> s <> "`."

            UnparseableTypeRef t ->
                "Invalid type: `" <> t <> "`."

            DuplicateKey _ dup -> do
                "Duplicated key: `" <> dup ^. keyLabel <> "`."
