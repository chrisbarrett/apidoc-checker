{-# LANGUAGE OverloadedStrings #-}
-- |Implements a JSON parser that approximately conforms to the spec.
module Apidoc.Json where

import           Apidoc.Pos
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
import qualified Text.Trifecta.Delta          as Delta

-- * Json AST
--
-- Each node in the parsed JSON AST is tagged with a 'Span' so that verification
-- steps can print error messages referring to the original input.

data Json = JObject Pos (Map Key Json)
          | JArray  Pos [Json]
          | JNumber Pos Double
          | JString Pos Text
          | JBool   Pos Bool
          | JNull   Pos
  deriving (Eq, Show)

data Key = Key {
    keyPos   :: !Pos
  , keyLabel :: !Text
  } deriving (Show, Eq, Ord)

-- * Parsers

parseFile :: MonadIO m => FilePath -> m (Either PP.Doc Json)
parseFile file =
    resultToEither <$> parseFromFileEx parseJson file


parseEither :: BS.ByteString -> Either PP.Doc Json
parseEither s =
    resultToEither (parseByteString parseJson mempty s)


parseJson :: Parser Json
parseJson = whitespace *> document <* whitespace
    where
      document =  parseNull
              <|> parseBool
              <|> parseNumber
              <|> parseString
              <|> parseArray
              <|> parseObject


whitespace :: Parser ()
whitespace =
    spaces <|> skipMany newline

parseArray :: Parser Json
parseArray =
    JArray
      <$> pos
      <*> brackets (whitespace >> commaSep parseJson)
    <?> "array"


parseObject :: Parser Json
parseObject =
    JObject
      <$> pos
      <*> (Map.fromList <$> braces (whitespace *> commaSep kvp))
    <?> "object"
  where
    kvp :: Parser (Key, Json)
    kvp = do
      (JString s label) <- whitespace *> parseString <* (whitespace >> char ':')
      json <- parseJson
      pure (Key s label, json)


parseBool :: Parser Json
parseBool =
    JBool
      <$> pos
      <*> (true <|> false)
    <?> "boolean"
  where
    true  = string "true"  *> pure True
    false = string "false" *> pure False


parseNull :: Parser Json
parseNull =
    JNull <$> pos <* text "null"


parseNumber :: Parser Json
parseNumber =
    JNumber
      <$> pos
      <*> (notUnaryPlus >> (zero <|> anyNumeric))
    <?> "number"
  where
    notUnaryPlus = lookAhead (try (noneOf "+"))
    zero = char '0' <* notFollowedBy (some digit) >> pure 0
    anyNumeric = Either.either fromIntegral id <$> integerOrDouble


parseString :: Parser Json
parseString = do
    JString
      <$> pos
      <*> (Text.pack <$> (start *> content `manyTill` end))
    <?> "string"
  where
    start = char '"' <?> "start of string (double-quotes)"
    content = escapeSequence <|> anyChar
    end = char '"' <?> "end of string (double-quotes)"

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

pos :: Parser Pos
pos = do
    p <- position
    case p of
      Delta.Lines l c _ _ ->
          pure (Pos (toInteger l) (toInteger c))

      Delta.Directed _ l c _ _ ->
          pure (Pos (toInteger l) (toInteger c))

      _ ->
          pure (Pos 0 0)

resultToEither :: Result a -> Either PP.Doc a
resultToEither (Success x) = Right x
resultToEither (Failure e) = Left e

typeOf :: Json -> Text
typeOf JObject {} = "object"
typeOf JArray  {} = "array"
typeOf JNumber {} = "number"
typeOf JString {} = "string"
typeOf JBool   {} = "bool"
typeOf JNull   {} = "null"
