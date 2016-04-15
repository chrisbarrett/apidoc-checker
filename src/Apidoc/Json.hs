{-# LANGUAGE OverloadedStrings #-}
-- |Implements a JSON parser that approximately conforms to the spec.
module Apidoc.Json where

import           Control.Applicative
import           Control.Monad.Trans          (MonadIO)
import qualified Data.ByteString              as BS
import qualified Data.Char                    as Char
import qualified Data.Either                  as Either
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import qualified Data.Maybe                   as Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Text.Parser.LookAhead
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Trifecta                hiding (parseString)

-- * Json AST
--
-- Each node in the parsed JSON AST is tagged with a 'Span' so that verification
-- steps can print error messages referring to the original input.

data Json s = JObject s (Map (Key s) (Json s))
            | JArray  s [Json s]
            | JNumber s Double
            | JString s Text
            | JBool   s Bool
            | JNull   s
  deriving (Eq, Show)

data Key s = Key {
    keySpan  :: !s
  , keyLabel :: !Text
  } deriving (Show, Eq, Ord)

-- * Parsers

parseFile :: MonadIO m => FilePath -> m (Result (Json Span))
parseFile = parseFromFileEx parseJson

parseEither :: BS.ByteString -> Either PP.Doc (Json Span)
parseEither s = resultToEither (parseByteString parseJson mempty s)


parseJson :: Parser (Json Span)
parseJson = whitespace *> document <* whitespace
    where
      document =  parseNull
              <|> parseBool
              <|> parseNumber
              <|> parseString
              <|> parseArray
              <|> parseObject

whitespace :: Parser ()
whitespace = spaces <|> skipMany newline

parseNumber, parseString, parseBool, parseNull, parseArray, parseObject :: Parser (Json Span)

parseArray = do
    (elems :~ s) <- spanned (brackets (whitespace >> commaSep parseJson))
    pure (JArray s elems)
    <?> "array"

parseObject = do
    (content :~ s) <- spanned (braces (whitespace *> commaSep kvp))
    pure (JObject s (Map.fromList content))
    <?> "object"
  where
    kvp :: Parser (Key Span, Json Span)
    kvp = do
      (JString s label) <- whitespace *> parseString <* (whitespace >> char ':')
      json <- parseJson
      return (Key s label, json)

parseBool = do
    let true  = string "true"  *> pure True
        false = string "false" *> pure False
    (b :~ s) <- spanned (true <|> false)
    pure (JBool s b)
    <?> "boolean"

parseNull = do
    s <- spanning (text "null")
    pure (JNull s)

parseNumber = do
    let notUnaryPlus = lookAhead (try (noneOf "+"))
        zero = char '0' <* notFollowedBy (some digit) >> pure 0
        anyNumeric = Either.either fromIntegral id <$> integerOrDouble
    (n :~ s) <- spanned (notUnaryPlus >> (zero <|> anyNumeric))
    pure (JNumber s n)
    <?> "number"

parseString = do
    let start = char '"' <?> "start of string (double-quotes)"
        content = escapeSequence <|> anyChar
        end = char '"' <?> "end of string (double-quotes)"
    (str :~ s) <- spanned (start *> content `manyTill` end)
    pure (JString s (Text.pack str))
    <?> "string"
  where
    escapeSequence :: Parser Char
    escapeSequence = do
        ch <- char '\\' *> anyChar
        let escapeCode = pure <$> Map.lookup ch escapeCodes
            unicodeChar = if ch == 'u' then unicode else fail "Invalid escape sequence."
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
        then fail "Invalid unicode sequence."
        else return $ Char.chr $ read $ "0x" ++ code


-- * Utility functions

resultToEither :: Result a -> Either PP.Doc a
resultToEither (Success x) = Right x
resultToEither (Failure e) = Left e

eraseSpans :: Json a -> Json ()
eraseSpans = mapSpans (const ())

mapSpans :: Ord b => (a -> b) -> Json a -> Json b
mapSpans f (JArray s xs) = JArray (f s) (mapSpans f <$> xs)
mapSpans f (JNumber s n) = JNumber (f s) n
mapSpans f (JString s t) = JString (f s) t
mapSpans f (JBool s b)   = JBool (f s) b
mapSpans f (JNull s)     = JNull (f s)
mapSpans f (JObject s m) = JObject (f s) (mapSpans f <$> mapKeySpans f m)

mapKeySpans :: Ord b => (a -> b) -> Map (Key a) v -> Map (Key b) v
mapKeySpans f = Map.mapKeys (\ (Key s t) -> Key (f s) t)

spanOf :: Json s -> s
spanOf (JObject s _) = s
spanOf (JArray  s _) = s
spanOf (JNumber s _) = s
spanOf (JString s _) = s
spanOf (JBool   s _) = s
spanOf (JNull   s)   = s

typeOf :: Json s -> Text
typeOf JObject {} = "object"
typeOf JArray  {} = "array"
typeOf JNumber {} = "number"
typeOf JString {} = "string"
typeOf JBool   {} = "bool"
typeOf JNull   {} = "null"
