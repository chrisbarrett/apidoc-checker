module Apidoc.Parser  where

import           Apidoc.DSL                   (Spec)
import           Apidoc.Json                  (Json)
import qualified Apidoc.Json                  as Json
import           Apidoc.Utils                 (resultToEither)
import           Data.ByteString              as BS
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Trifecta

parse :: BS.ByteString -> Either PP.Doc Spec
parse s = do
    json <- Json.parse s
    resultToEither (parseSpec json)

parseSpec :: Json -> Result Spec
parseSpec = undefined
