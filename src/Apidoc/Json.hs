{-# LANGUAGE OverloadedStrings #-}
-- |Implements a JSON parser that approximately conforms to the spec.
module Apidoc.Json where

import           Apidoc.Utils                 (resultToEither)
import           Control.Applicative
import           Control.Monad                (when)
import qualified Data.ByteString              as BS
import qualified Data.Char                    as Char
import qualified Data.Either                  as Either
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import qualified Data.Maybe                   as Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Text.Parser.LookAhead
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Trifecta                hiding (parseString)
import           Text.Trifecta.Delta

data Json = JObject !(Map Text Json)
          | JArray ![Json]
          | JNumber !Double
          | JString !Text
          | JBool !Bool
          | JNull
  deriving (Eq, Show)

parse :: BS.ByteString -> Either PP.Doc Json
parse s = resultToEither (parseByteString (fst <$> parseJson) mempty s)

parseJson :: Parser (Json, Delta)
parseJson = whitespace *> document <* whitespace
    where
      document =  parseNull
              <|> parseBool
              <|> parseNumber
              <|> parseString
              <|> parseArray
              <|> parseObject
              <?> "value"

whitespace :: Parser ()
whitespace = spaces <|> skipMany newline <?> "whitespace"

parseNumber, parseString, parseBool, parseNull, parseArray, parseObject :: Parser (Json, Delta)

parseArray = do
    pos <- position
    elems <- brackets (whitespace *> commaSep (fst <$> parseJson))
    pure (JArray elems, pos)

parseObject = do
    pos <- position
    content <- braces (whitespace *> commaSep kvp)
    pure (JObject (Map.fromList content), pos)
  where
    kvp = do
      (JString label, _) <- whitespace *> parseString <* (whitespace >> char ':')
      json <- fst <$> parseJson
      return (label, json)

parseBool = (do
    pos <- position
    b <- (string "true" *> pure True) <|> (string "false" *> pure False)
    pure (JBool b, pos))
    <?> "boolean"

parseNull = do
    pos <- position
    text "null" *> pure (JNull, pos)

parseNumber = do
    pos <- position
    let notUnaryPlus = lookAhead (try (noneOf "+"))
        zero = char '0' <* notFollowedBy (some digit) >> pure 0
        anyNumeric = Either.either fromIntegral id <$> integerOrDouble
    n <- notUnaryPlus >> (zero <|> anyNumeric)
    pure (JNumber n, pos)

parseString = do
    pos <- position
    let start = char '"' <?> "start of string (double-quotes)"
        content = escapeSequence <|> anyChar
        end = char '"' <?> "end of string (double-quotes)"
    str <- start *> content `manyTill` end
    pure (JString (Text.pack str), pos)
  where
    escapeSequence :: Parser Char
    escapeSequence = do
        ch <- char '\\' *> anyChar
        let escapeCode = pure <$> Map.lookup ch escapeCodes
            unicodeChar = if ch == 'u' then unicode else fail "invalid escape sequence"
        Maybe.fromMaybe unicodeChar escapeCode

    escapeCodes = Map.fromList
                  [ ('\"', '\"')
                  , ('\\', '\\')
                  , ('/', '/')
                  , ('n', '\n')
                  , ('r', '\r')
                  , ('f', '\f')
                  , ('t', '\t')
                  , ('b', '\b')
                  ]

    unicode = do
      code <- count 4 hexDigit
      if null code
        then fail "invalid unicode sequence"
        else return $ Char.chr $ read $ "0x" ++ code
