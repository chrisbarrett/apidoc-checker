module Apidoc.Check.Parsers where

import qualified Apidoc.DSL          as DSL
import           Control.Applicative
import           Control.Monad
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Text.Trifecta

parseTypeRef :: MonadPlus m => Text -> m DSL.TypeRef
parseTypeRef s =
  case parseString parser mempty (Text.unpack s) of
    Success x -> pure x
    Failure _ -> empty
  where
    parser :: Parser DSL.TypeRef
    parser = choice [arrayType, mapType, nominalType]

    bracketed = between (char '[') (char ']')

    arrayType = bracketed parser
    mapType   = string "map" *> bracketed parser

    nominalType = do
        s <- concat <$> identifier `sepBy1` char '.'
        pure (DSL.TNominal (DSL.TypeName (Text.pack s)))

    underscore = char '_'
    identifier = do
        c <- letter <|> underscore
        cs <- many (alphaNum <|> underscore)
        pure (c : cs)
