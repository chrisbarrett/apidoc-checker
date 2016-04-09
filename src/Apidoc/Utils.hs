module Apidoc.Utils where

import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Trifecta

resultToEither :: Result a -> Either PP.Doc a
resultToEither (Success x) = Right x
resultToEither (Failure e) = Left e
