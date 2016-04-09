{-# LANGUAGE OverloadedStrings #-}
-- |Implements parsing and verification of apidoc specs.
module Apidoc.Parser  where

import qualified Apidoc.DSL                   as DSL
import           Apidoc.Json
import qualified Apidoc.Json                  as Json
import           Data.ByteString              as BS
import           Data.Map.Strict              (Map)
import qualified Data.Maybe                   as Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Network.URI                  as URI
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Read                    (readMaybe)
import           Text.Trifecta                (Span)

type Parser a = Json Span -> Either PP.Doc a
type KeyParser a = Key Span -> Either PP.Doc a

-- * Parsers

-- |Main entrypoint for the module. Parses a 'ByteString' to a validated
-- representation of an apidoc spec. Any errors are collected for pretty
-- printing.
parse :: BS.ByteString -> Either PP.Doc DSL.Spec
parse s = Json.parse s >>= parseSpec

parseSpec :: Parser DSL.Spec
parseSpec js =
    DSL.Spec
      <$> optional "apidoc" apidoc js
      <*> optional "attributes" (array attribute) js
      <*> optional "baseUrl" uri js
      <*> optional "description" text js
      <*> optional "enums" (object (keyType typeName) enum) js
      <*> optional "headers" (array header) js
      <*> optional "imports" (array import_) js
      <*> optional "info" info js
      <*> optional "models" (object (keyType typeName) model) js
      <*> required "name" serviceName js
      <*> optional "namespace" namespace js
      <*> optional "resources" (object (keyType typeRef) resource) js
      <*> optional "unions" (object (keyType typeName) union) js

enum :: Parser DSL.Enum
enum js =
    DSL.Enum
      <$> optional "attributes" (array attribute) js
      <*> optional "deprecation" deprecation js
      <*> optional "description" text js
      <*> optional "plural" typeName js
      <*> required "description" (array parseEnumValue) js

parseEnumValue :: Parser DSL.EnumValue
parseEnumValue js =
    DSL.EnumValue
      <$> optional "attributes" (array attribute) js
      <*> optional "deprecation" deprecation js
      <*> optional "description" text js
      <*> required "name" typeName js

header :: Parser DSL.Header
header js =
    DSL.Header
      <$> optional "attributes" (array attribute) js
      <*> optional "default" text js
      <*> optional "deprecation" deprecation js
      <*> optional "description" text js
      <*> required "name" text js
      <*> optional "required" bool js
      <*> required "type" typeRef js

import_ :: Parser DSL.Import
import_ js =
    DSL.Import
      <$> required "uri" uri js

info :: Parser DSL.Info
info js =
    DSL.Info
      <$> optional "license" license js
      <*> optional "contact" contact js

license :: Parser DSL.License
license js =
    DSL.License
      <$> required "name" text js
      <*> optional "url" uri js

contact :: Parser DSL.Contact
contact js =
    DSL.Contact
      <$> optional "email" text js
      <*> optional "name" text js
      <*> optional "url" uri js

model :: Parser DSL.Model
model js =
    DSL.Model
      <$> optional "attributes" (array attribute) js
      <*> optional "deprecation" deprecation js
      <*> optional "description" text js
      <*> required "fields" (array parseField) js
      <*> optional "plural" typeName js

parseField :: Parser DSL.Field
parseField js =
    DSL.Field
      <$> optional "attributes" (array attribute) js
      <*> optional "default" text js
      <*> optional "deprecation" deprecation js
      <*> optional "description" text js
      <*> optional "example" text js
      <*> optional "minimum" natural js
      <*> optional "maximum" natural js
      <*> required "name" fieldName js
      <*> optional "required" bool js
      <*> required "type" typeRef js

resource :: Parser DSL.Resource
resource js =
    DSL.Resource
      <$> optional "attributes" (array attribute) js
      <*> optional "deprecation" deprecation js
      <*> optional "description" text js
      <*> required "operations" (array operation) js
      <*> optional "path" text js

operation :: Parser DSL.Operation
operation js =
    DSL.Operation
      <$> optional "attributes" (array attribute) js
      <*> optional "body" body js
      <*> optional "deprecation" deprecation js
      <*> optional "description" text js
      <*> required "method" httpMethod js
      <*> optional "parameters" (array parameter) js
      <*> optional "path" text js
      <*> optional "responses" (object (keyType responseCode) response) js

body :: Parser DSL.Body
body js =
    DSL.Body
      <$> optional "attributes" (array attribute) js
      <*> optional "deprecation" deprecation js
      <*> optional "description" text js
      <*> required "type" typeRef js

parameter :: Parser DSL.Parameter
parameter js =
    DSL.Parameter
      <$> optional "default" anyJson js
      <*> optional "deprecation" deprecation js
      <*> optional "description" text js
      <*> optional "example" text js
      <*> optional "location" parameterLocation js
      <*> optional "maximum" natural js
      <*> optional "minimum" natural js
      <*> required "name" text js
      <*> optional "required" bool js
      <*> required "type" typeRef js

parameterLocation :: Parser DSL.ParameterLocation
parameterLocation js = do
    str <- text js
    case str of
        "path"  -> pure DSL.Path
        "query" -> pure DSL.Query
        "form"  -> pure DSL.Form
        _       -> fail "Expected \"path\", \"query\" or \"form\"."

httpMethod :: Parser DSL.HttpMethod
httpMethod js = do
    str <- text js
    case str of
        "GET"     -> pure DSL.GET
        "POST"    -> pure DSL.POST
        "PUT"     -> pure DSL.PUT
        "PATCH"   -> pure DSL.PATCH
        "DELETE"  -> pure DSL.DELETE
        "HEAD"    -> pure DSL.HEAD
        "CONNECT" -> pure DSL.CONNECT
        "OPTIONS" -> pure DSL.OPTIONS
        "TRACE"   -> pure DSL.TRACE
        _         -> fail "Expected an HTTP method."

responseCode :: Parser DSL.ResponseCode
responseCode js = do
    str <- text js
    case (str, readMaybe (Text.unpack str)) of
        ("default", _) -> pure DSL.RespDefault
        (_, Just n)    -> pure (DSL.RespInt n)
        _              -> fail "Expected a valid HTTP status code or \"default\"."

response :: Parser DSL.Response
response js =
    DSL.Response
      <$> optional "deprecation" deprecation js
      <*> optional "description" text js
      <*> required "type" typeRef js

union :: Parser DSL.Union
union js =
    DSL.Union
      <$> optional "attributes" (array attribute) js
      <*> optional "deprecation" deprecation js
      <*> optional "description" text js
      <*> optional "discriminator" text js
      <*> required "name" typeName js
      <*> optional "plural" typeName js
      <*> required "types" (array unionType) js

unionType :: Parser DSL.UnionType
unionType js =
    DSL.UnionType
      <$> optional "attributes" (array attribute) js
      <*> optional "deprecation" deprecation js
      <*> optional "description" text js
      <*> required "type" typeRef js

typeName :: Parser DSL.TypeName
typeName js =
    DSL.TypeName <$> text js

fieldName :: Parser DSL.FieldName
fieldName js =
    DSL.FieldName <$> text js

namespace :: Parser DSL.Namespace
namespace js =
    DSL.Namespace <$> text js

serviceName :: Parser DSL.ServiceName
serviceName js =
    DSL.ServiceName <$> text js

apidoc :: Parser DSL.Apidoc
apidoc js =
    DSL.Apidoc <$> required "version" text js

attribute :: Parser DSL.Attribute
attribute js =
    DSL.Attribute
      <$> required "name" text js
      <*> required "value" parseJObject js

deprecation :: Parser DSL.Deprecation
deprecation js =
    DSL.Deprecation <$> optional "description" text js

text :: Parser Text
text (Json.JString _ s) = pure s
text _ = fail "expected string"

bool :: Parser Bool
bool (Json.JBool _ b) = pure b
bool _                = fail "expected boolean"

uri :: Parser DSL.Uri
uri js = do
    s <- Text.unpack <$> text js
    url <- Maybe.fromMaybe (fail "URI") (pure <$> URI.parseURI s)
    pure (DSL.Uri url)

parseJObject :: Parser (Json ())
parseJObject o@Json.JObject {} = pure (Json.eraseSpans o)
parseJObject _ = fail "Expected an object."

typeRef :: Parser DSL.TypeRef
typeRef js = undefined -- remote <|> builtin <|> local
  where
    remote = DSL.TRRemote <$> namespace js <*> typeName js
    local = DSL.TRLocal <$> typeName js
    builtin = DSL.TRBuiltIn <$> builtInType js

builtInType :: Parser DSL.TBuiltIn
builtInType js = do
    res <- text js
    case res of
        "date-iso8601" -> pure DSL.TDateIso8601
        "date-time-iso8601" -> pure DSL.TDateTimeIso8601
        "boolean" -> pure DSL.TBoolean
        "decimal" -> pure DSL.TDecimal
        "integer" -> pure DSL.TInteger
        "long"    -> pure DSL.TLong
        "string"  -> pure DSL.TString
        "unit"    -> pure DSL.TUnit
        "uuid"    -> pure DSL.TUuid
        _         -> fail "Not a built-in type"


-- * Utilities

keyType :: Parser a -> KeyParser a
keyType = undefined

array :: Parser a -> Parser [a]
array p (JArray _ xs) = sequence (p <$> xs)
array _ _             = fail "Expected an array."

object :: KeyParser k -> Parser v -> Parser (Map k v)
object pk pv (JObject _ m) = undefined
object _ _ _ = fail "Expected an object."

required :: Text -> Parser a -> Parser a
required js = undefined

optional :: Text -> Parser a -> Parser (Maybe a)
optional js = undefined

anyJson :: Parser (Json ())
anyJson = pure . Json.eraseSpans

natural :: Parser Integer
natural (Json.JNumber _ n)
    | n >= 0 && n == fromInteger (round n) = pure (round n)
natural _ = fail "Expected a whole number 0 or greater."
