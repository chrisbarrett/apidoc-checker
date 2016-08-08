{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Apidoc.Json.Parser where

import           Apidoc.Json.Types
import           Apidoc.Pos
import           Control.Applicative
import           Control.Monad.Trans          (MonadIO)
import qualified Data.ByteString              as BS
import qualified Data.Char                    as Char
import qualified Data.Either                  as Either
import qualified Data.Map.Strict              as Map
import qualified Data.Maybe                   as Maybe
import           Data.Sequence                (Seq)
import qualified Data.Sequence                as Seq
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Text.Parser.LookAhead
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Trifecta                hiding (parseString)
import qualified Text.Trifecta.Delta          as Delta

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
      <*> brackets (whitespace *> (Seq.fromList <$> commaSep parseJson))
    <?> "array"


parseObject :: Parser Json
parseObject = do
    p <- pos
    content <- braces (whitespace *> commaSep kvp)
    pure (JObject p (Object p (Seq.fromList content)))
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

    escapeCodes = [ ('\"', '\"')
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

jsonPos :: Json -> Pos
jsonPos (JObject p _) = p
jsonPos (JArray  p _) = p
jsonPos (JNumber p _) = p
jsonPos (JString p _) = p
jsonPos (JBool   p _) = p
jsonPos (JNull   p) = p
