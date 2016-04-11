{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |Implements parsing and verification of apidoc specs.
module Apidoc.Parser  where

import qualified Apidoc.DSL                   as DSL
import           Apidoc.Json
import qualified Apidoc.Json                  as Json
import qualified Data.ByteString              as BS
import qualified Data.Map                     as Map
import           Data.Map.Strict              (Map)
import qualified Data.Maybe                   as Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Tuple                   as Tuple
import           Data.Validation              (Validation)
import qualified Data.Validation              as Validation
import qualified Network.URI                  as URI
import           Prelude                      hiding (span)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Read                    (readMaybe)
import           Text.Trifecta                (Span (..))
import qualified Text.Trifecta                as Trifecta

type Result = Validation PP.Doc
type Validator a = Json Span -> Result a
type KeyValidator a = Key Span -> Result a

-- * Parsers

-- |Main entrypoint for the module. Parses a 'ByteString' to a validated
-- representation of an apidoc spec. Any errors are collected for pretty
-- printing.
parse :: BS.ByteString -> Result DSL.Spec
parse s =
    case Json.parse s of
      Trifecta.Success js -> parseSpec js
      Trifecta.Failure e  -> Validation.Failure e

parseSpec :: Validator DSL.Spec
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

enum :: Validator DSL.Enum
enum js =
    DSL.Enum
      <$> optional "attributes" (array attribute) js
      <*> optional "deprecation" deprecation js
      <*> optional "description" text js
      <*> optional "plural" typeName js
      <*> required "values" (array parseEnumValue) js

parseEnumValue :: Validator DSL.EnumValue
parseEnumValue js =
    DSL.EnumValue
      <$> optional "attributes" (array attribute) js
      <*> optional "deprecation" deprecation js
      <*> optional "description" text js
      <*> required "name" typeName js

header :: Validator DSL.Header
header js =
    DSL.Header
      <$> optional "attributes" (array attribute) js
      <*> optional "default" text js
      <*> optional "deprecation" deprecation js
      <*> optional "description" text js
      <*> required "name" text js
      <*> optional "required" bool js
      <*> required "type" typeRef js

import_ :: Validator DSL.Import
import_ js =
    DSL.Import
      <$> required "uri" uri js

info :: Validator DSL.Info
info js =
    DSL.Info
      <$> optional "license" license js
      <*> optional "contact" contact js

license :: Validator DSL.License
license js =
    DSL.License
      <$> required "name" text js
      <*> optional "url" uri js

contact :: Validator DSL.Contact
contact js =
    DSL.Contact
      <$> optional "email" text js
      <*> optional "name" text js
      <*> optional "url" uri js

model :: Validator DSL.Model
model js =
    DSL.Model
      <$> optional "attributes" (array attribute) js
      <*> optional "deprecation" deprecation js
      <*> optional "description" text js
      <*> required "fields" (array parseField) js
      <*> optional "plural" typeName js

parseField :: Validator DSL.Field
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

resource :: Validator DSL.Resource
resource js =
    DSL.Resource
      <$> optional "attributes" (array attribute) js
      <*> optional "deprecation" deprecation js
      <*> optional "description" text js
      <*> required "operations" (array operation) js
      <*> optional "path" text js

operation :: Validator DSL.Operation
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

body :: Validator DSL.Body
body js =
    DSL.Body
      <$> optional "attributes" (array attribute) js
      <*> optional "deprecation" deprecation js
      <*> optional "description" text js
      <*> required "type" typeRef js

parameter :: Validator DSL.Parameter
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

parameterLocation :: Validator DSL.ParameterLocation
parameterLocation js = do
    str <- text js
    case str of
        "path"  -> pure DSL.Path
        "query" -> pure DSL.Query
        "form"  -> pure DSL.Form
        _       -> raiseError "Expected \"path\", \"query\" or \"form\"." (spanOf js)

httpMethod :: Validator DSL.HttpMethod
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
        _         -> raiseError "Expected an HTTP method." (spanOf js)

responseCode :: Validator DSL.ResponseCode
responseCode js = do
    str <- text js
    case (str, readMaybe (Text.unpack str)) of
        ("default", _) -> pure DSL.RespDefault
        (_, Just n)    -> pure (DSL.RespInt n)
        _              -> raiseError "Expected a valid HTTP status code or \"default\"." (spanOf js)

response :: Validator DSL.Response
response js =
    DSL.Response
      <$> optional "deprecation" deprecation js
      <*> optional "description" text js
      <*> required "type" typeRef js

union :: Validator DSL.Union
union js =
    DSL.Union
      <$> optional "attributes" (array attribute) js
      <*> optional "deprecation" deprecation js
      <*> optional "description" text js
      <*> optional "discriminator" text js
      <*> optional "plural" typeName js
      <*> required "types" (array unionType) js

unionType :: Validator DSL.UnionType
unionType js =
    DSL.UnionType
      <$> optional "attributes" (array attribute) js
      <*> optional "deprecation" deprecation js
      <*> optional "description" text js
      <*> required "type" typeRef js

typeName :: Validator DSL.TypeName
typeName js =
    DSL.TypeName <$> text js

fieldName :: Validator DSL.FieldName
fieldName js =
    DSL.FieldName <$> text js

namespace :: Validator DSL.Namespace
namespace js =
    DSL.Namespace <$> text js

serviceName :: Validator DSL.ServiceName
serviceName js =
    DSL.ServiceName <$> text js

apidoc :: Validator DSL.Apidoc
apidoc js =
    DSL.Apidoc <$> required "version" text js

attribute :: Validator DSL.Attribute
attribute js =
    DSL.Attribute
      <$> required "name" text js
      <*> required "value" parseJObject js

deprecation :: Validator DSL.Deprecation
deprecation js =
    DSL.Deprecation <$> optional "description" text js


-- TODO: parse different kinds of reference types.
typeRef :: Validator DSL.TypeRef
typeRef js = DSL.TRLocal <$> typeName js

builtInType :: Validator DSL.TBuiltIn
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
        s -> typeError (Expected "built-in type") (Actual s) (spanOf js)

-- * Notes

raiseError :: Text -> Span -> Result a
raiseError msg (Span start end t) =
    let caret = Trifecta.renderingCaret start t
        span = Trifecta.addSpan start end caret
        err = Trifecta.failed (Text.unpack msg)
    in
      Validation.Failure $ Trifecta.explain span err

requiredKeyMissing :: Text -> Span -> Result a
requiredKeyMissing k (Span start _ t) =
    let caret = Trifecta.renderingCaret start t
        msg = Text.concat ["Object does not have required key \"" , k , "\""]
        err = Trifecta.failed (Text.unpack msg)
    in
      Validation.Failure $ Trifecta.explain caret err

newtype Expected = Expected Text
newtype Actual = Actual Text

typeError :: Expected -> Actual -> Span -> Result a
typeError (Expected expected) (Actual actual) (Span start end t) =
    let caret = Trifecta.renderingCaret start t
        span = Trifecta.addSpan start end caret
        msg = Text.concat ["Type error: Expected \"", expected, "\", but got \"", actual, "\""]
        err = Trifecta.failed (Text.unpack msg)
    in
      Validation.Failure $ Trifecta.explain span err

-- * Utilities

-- |Map a parser over JSON strings to parsing JSON object keys.
keyType :: Validator a -> KeyValidator a
keyType p (Key s t) = p (JString s t)

array :: Validator a -> Validator [a]
array p (JArray _ xs) = mapM p xs
array _ js = typeError (Expected "array") (Actual (typeOf js)) (spanOf js)

object :: Ord k => KeyValidator k -> Validator v -> Validator (Map k v)
object pk pv (JObject _ m) = mapKeysM pk m >>= traverse pv
object _ _ js = typeError (Expected "object") (Actual (typeOf js)) (spanOf js)

text :: Validator Text
text (Json.JString _ s) = pure s
text js = typeError (Expected "string") (Actual (typeOf js)) (spanOf js)

bool :: Validator Bool
bool (Json.JBool _ b) = pure b
bool js = typeError (Expected "boolean") (Actual (typeOf js)) (spanOf js)

uri :: Validator DSL.Uri
uri js = do
    s <- Text.unpack <$> text js
    url <- Maybe.fromMaybe (raiseError "Invalid URI" (spanOf js)) (pure <$> URI.parseURI s)
    pure (DSL.Uri url)

parseJObject :: Validator (Json ())
parseJObject o@Json.JObject {} = pure (Json.eraseSpans o)
parseJObject js = typeError (Expected "object") (Actual (typeOf js)) (spanOf js)

mapKeysM :: (Monad m, Ord b) => (a -> m b) -> Map a v -> m (Map b v)
mapKeysM f m = do
    let swapped = Tuple.swap <$> Map.toList m
    xs <- (traverse.traverse) f swapped
    pure (Map.fromList (Tuple.swap <$> xs))

required :: Text -> Validator a -> Validator a
required k p (JObject s m) =
    let stringKeys = Map.mapKeys keyLabel m
    in case Map.lookup k stringKeys of
         Just js -> p js
         Nothing -> requiredKeyMissing k s

required _  _  js = typeError (Expected "object") (Actual (typeOf js)) (spanOf js)


optional :: Text -> Validator a -> Validator (Maybe a)
optional k p (JObject _ m) =
    let stringKeys = Map.mapKeys keyLabel m
    in case Map.lookup k stringKeys of
         Just js -> Just <$> p js
         Nothing -> pure Nothing

optional _  _  js = typeError (Expected "object") (Actual (typeOf js)) (spanOf js)

anyJson :: Validator (Json ())
anyJson = pure . Json.eraseSpans

natural :: Validator Integer
natural (Json.JNumber _ n)
    | n >= 0 && n == fromInteger (round n) = pure (round n)
natural js = raiseError "Expected a whole number 0 or greater" (spanOf js)
