{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}
-- |Implements parsing and verification of apidoc specs.
module Apidoc.Parser (

      -- |Parses the given file to a validated representation of an apidoc spec.
      -- Any errors are collected for pretty printing.
      Apidoc.Parser.parseFile

    ) where

import qualified Apidoc.DSL                   as DSL
import           Apidoc.Json
import qualified Apidoc.Json                  as Json
import           Control.Applicative          hiding (optional)
import           Control.Lens                 hiding (enum, swapped)
import           Control.Monad.Reader         (MonadReader, ReaderT (..))
import qualified Control.Monad.Reader         as Reader
import           Control.Monad.State.Strict   (MonadState, StateT (..))
import qualified Control.Monad.State.Strict   as State
import           Control.Monad.Trans          (MonadIO)
import qualified Data.Maybe                   as Maybe

import qualified Data.List                    as List
import qualified Data.Map                     as Map
import           Data.Map.Strict              (Map)
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Validation              (Validation, _Failure)
import qualified Network.URI                  as URI
import           Prelude                      hiding (span)
import qualified Text.EditDistance            as EditDistance
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Read                    (readMaybe)
import           Text.Trifecta                (Span (..))
import qualified Text.Trifecta                as Trifecta


type Err = PP.Doc
type Result = Validation Err
type Validator a = Json Span -> Result a
type KeyValidator a = Key Span  -> Result a


newtype DslValidator a = DslValidator {
    _unvalidator :: StateT [Text]
                          (ReaderT (Json Span) Result)
                          a
    } deriving (Functor, Applicative, Monad, MonadReader (Json Span), MonadState [Text])

runValidator :: DslValidator a -> Json Span -> Result (a, [Text])
runValidator p = Reader.runReaderT (State.runStateT (_unvalidator p) [])

-- * Parsers

parseFile :: MonadIO m => FilePath -> m (Validation Err DSL.Spec)
parseFile fp = do
    js <- Json.parseFile fp
    case js of
      Trifecta.Success js' -> pure (parseSpec js')
      Trifecta.Failure e  -> pure (_Failure # e)

parseSpec :: Validator DSL.Spec
parseSpec =
    validator $ DSL.Spec
      <$> optional "apidoc" apidoc
      <*> optional "attributes" (array attribute)
      <*> optional "base_url" uri
      <*> optional "description" text
      <*> optional "enums" (object (keyType typeName) enum)
      <*> optional "headers" (array header)
      <*> optional "imports" (array import_)
      <*> optional "info" info
      <*> optional "models" (object (keyType typeName) model)
      <*> required "name" serviceName
      <*> optional "namespace" namespace
      <*> optional "resources" (object (keyType typeRef) resource)
      <*> optional "unions" (object (keyType typeName) union)

enum :: Validator DSL.Enum
enum =
    validator $ DSL.Enum
      <$> optional "attributes" (array attribute)
      <*> optional "deprecation" deprecation
      <*> optional "description" text
      <*> optional "plural" typeName
      <*> required "values" (array enumValue)

enumValue :: Validator DSL.EnumValue
enumValue =
    validator $ DSL.EnumValue
      <$> optional "attributes" (array attribute)
      <*> optional "deprecation" deprecation
      <*> optional "description" text
      <*> required "name" typeName

header :: Validator DSL.Header
header =
    validator $ DSL.Header
      <$> optional "attributes" (array attribute)
      <*> optional "default" text
      <*> optional "deprecation" deprecation
      <*> optional "description" text
      <*> required "name" text
      <*> optional "required" bool
      <*> required "type" typeRef

import_ :: Validator DSL.Import
import_ =
    validator $ DSL.Import
      <$> required "uri" uri

info :: Validator DSL.Info
info =
    validator $ DSL.Info
      <$> optional "license" license
      <*> optional "contact" contact

license :: Validator DSL.License
license =
    validator $ DSL.License
      <$> required "name" text
      <*> optional "url" uri

contact :: Validator DSL.Contact
contact =
    validator $ DSL.Contact
      <$> optional "email" text
      <*> optional "name" text
      <*> optional "url" uri

model :: Validator DSL.Model
model =
    validator $ DSL.Model
      <$> optional "attributes" (array attribute)
      <*> optional "deprecation" deprecation
      <*> optional "description" text
      <*> required "fields" (array modelField)
      <*> optional "plural" typeName

modelField :: Validator DSL.Field
modelField =
    validator $ DSL.Field
      <$> optional "attributes" (array attribute)
      <*> optional "default" text
      <*> optional "deprecation" deprecation
      <*> optional "description" text
      <*> optional "example" text
      <*> optional "minimum" naturalNumber
      <*> optional "maximum" naturalNumber
      <*> required "name" fieldName
      <*> optional "required" bool
      <*> required "type" typeRef

resource :: Validator DSL.Resource
resource =
    validator $ DSL.Resource
      <$> optional "attributes" (array attribute)
      <*> optional "deprecation" deprecation
      <*> optional "description" text
      <*> required "operations" (array operation)
      <*> optional "path" text

operation :: Validator DSL.Operation
operation =
    validator $ DSL.Operation
      <$> optional "attributes" (array attribute)
      <*> optional "body" body
      <*> optional "deprecation" deprecation
      <*> optional "description" text
      <*> required "method" httpMethod
      <*> optional "parameters" (array parameter)
      <*> optional "path" text
      <*> optional "responses" (object (keyType responseCode) response)

body :: Validator DSL.Body
body =
    validator $ DSL.Body
      <$> optional "attributes" (array attribute)
      <*> optional "deprecation" deprecation
      <*> optional "description" text
      <*> required "type" typeRef

parameter :: Validator DSL.Parameter
parameter =
    validator $ DSL.Parameter
      <$> optional "default" anyJson
      <*> optional "deprecation" deprecation
      <*> optional "description" text
      <*> optional "example" text
      <*> optional "location" parameterLocation
      <*> optional "maximum" naturalNumber
      <*> optional "minimum" naturalNumber
      <*> required "name" text
      <*> optional "required" bool
      <*> required "type" typeRef

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
response =
    validator $ DSL.Response
      <$> optional "deprecation" deprecation
      <*> optional "description" text
      <*> required "type" typeRef

union :: Validator DSL.Union
union =
    validator $ DSL.Union
      <$> optional "attributes" (array attribute)
      <*> optional "deprecation" deprecation
      <*> optional "description" text
      <*> optional "discriminator" text
      <*> optional "plural" typeName
      <*> required "types" (array unionType)

unionType :: Validator DSL.UnionType
unionType =
    validator $ DSL.UnionType
      <$> optional "attributes" (array attribute)
      <*> optional "deprecation" deprecation
      <*> optional "description" text
      <*> required "type" typeRef

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
apidoc =
    validator $ DSL.Apidoc <$> required "version" text

attribute :: Validator DSL.Attribute
attribute =
    validator $ DSL.Attribute
      <$> required "name" text
      <*> required "value" parseJObject

deprecation :: Validator DSL.Deprecation
deprecation =
    validator $ DSL.Deprecation <$> optional "description" text

-- TODO: parse different kinds of reference types.
typeRef :: Validator DSL.TypeRef
typeRef js = DSL.TRLocal <$> typeName js


-- * Message output

raiseError :: Text -> Span -> Result a
raiseError msg (Span start end t) =
    let caret = Trifecta.renderingCaret start t
        span = Trifecta.addSpan start end caret
        err = Trifecta.failed (Text.unpack msg)
    in
      _Failure # (Trifecta.explain span err <> newlines)


requiredKeyMissing :: Text -> Span -> Result a
requiredKeyMissing k (Span start _ t) =
    let caret = Trifecta.renderingCaret start t
        msg = Text.concat ["Object does not have required key \"" , k , "\"."]
        err = Trifecta.failed (Text.unpack msg)
    in
      _Failure # (Trifecta.explain caret err <> newlines)


unexpectedKey :: Key Span -> Candidates -> Result a
unexpectedKey (Key (Span start _ t) k) cs =
    let caret = Trifecta.renderingCaret start t
        msg = Text.concat ["Unexpected key: \"" , k , "\".", suggestionClause k cs]
        err = Trifecta.failed (Text.unpack msg)
    in
      _Failure # (Trifecta.explain caret err <> newlines)


newtype Candidates = Candidates [Text]

suggestionClause :: Text -> Candidates -> Text
suggestionClause t (Candidates cs) =
    let distances = map (\s -> (distance t s, s)) cs
        best = Maybe.listToMaybe $ List.sortOn fst distances
    in case best of
         Just (score, c) | score < maximumEditDistance -> Text.concat [" Did you mean \"", c, "\"?"]
         _ -> ""
  where
    maximumEditDistance = 5

    distance (Text.unpack -> t1) (Text.unpack -> t2) =
        EditDistance.restrictedDamerauLevenshteinDistance EditDistance.defaultEditCosts t1 t2

newtype Expected = Expected Text
newtype Actual = Actual Text

typeError :: Expected -> Actual -> Span -> Result a
typeError (Expected expected) (Actual actual) (Span start end t) =
    let caret = Trifecta.renderingCaret start t
        span = Trifecta.addSpan start end caret
        msg = Text.concat ["Expected \"", expected, "\", but got \"", actual, "\"."]
        err = Trifecta.failed (Text.unpack msg)
    in
      _Failure # (Trifecta.explain span err <> newlines)


newlines :: PP.Doc
newlines = PP.line <> PP.line

-- * Utilities

-- |Map a parser over JSON strings to parsing JSON object keys.
keyType :: Validator a -> KeyValidator a
keyType p (Key s t) = p (JString s t)

array :: Validator a -> Validator [a]
array p (JArray _ xs) =
    traverse p xs
array _ js =
    typeError (Expected "array") (Actual (typeOf js)) (spanOf js)

object :: Ord k => KeyValidator k -> Validator v -> Validator (Map k v)
object pk pv (JObject _ m) =
    Map.fromList <$> traverse validateKvp (Map.toList m)
  where
    validateKvp (k, v) = (,) <$> pk k <*> pv v

object _ _ js =
    typeError (Expected "object") (Actual (typeOf js)) (spanOf js)

text :: Validator Text
text (Json.JString _ s) = pure s
text js =
    typeError (Expected "string") (Actual (typeOf js)) (spanOf js)

bool :: Validator Bool
bool (Json.JBool _ b) = pure b
bool js =
    typeError (Expected "boolean") (Actual (typeOf js)) (spanOf js)

uri :: Validator DSL.Uri
uri js = do
    str <- Text.unpack <$> text js
    Maybe.fromMaybe (raiseError "Invalid URI" (spanOf js))
                    (pure . DSL.Uri <$> URI.parseURI str)

parseJObject :: Validator (Json ())
parseJObject o@Json.JObject {} =
    pure (Json.eraseSpans o)
parseJObject js =
    typeError (Expected "object") (Actual (typeOf js)) (spanOf js)

required :: Text -> Validator a -> DslValidator a
required k p = do
    State.modify ((:) k)
    inputJs <- Reader.ask
    case inputJs of
      JObject s m -> do
        let stringKeys = Map.mapKeys keyLabel m
        case Map.lookup k stringKeys of
          Just js -> liftValidation $ p js
          Nothing -> liftValidation $ requiredKeyMissing k s
      _ ->
          liftValidation $ typeError (Expected "object") (Actual (typeOf inputJs)) (spanOf inputJs)

optional :: Text -> Validator a -> DslValidator (Maybe a)
optional k p = do
    State.modify ((:) k)
    inputJs <- Reader.ask
    case inputJs of
      JObject _ m -> do
        let stringKeys = Map.mapKeys keyLabel m
        case Map.lookup k stringKeys of
          Just js -> liftValidation $ Just <$> p js
          Nothing -> pure Nothing
      _ ->
          liftValidation $ typeError (Expected "object") (Actual (typeOf inputJs)) (spanOf inputJs)

liftValidation :: Validation Err a -> DslValidator a
liftValidation = DslValidator . State.lift . Reader.lift

anyJson :: Validator (Json ())
anyJson = pure . Json.eraseSpans

naturalNumber :: Validator Integer
naturalNumber (Json.JNumber _ n)
    | n >= 0 && n == fromInteger (round n) = pure (round n)
naturalNumber js =
    raiseError "Expected a whole number 0 or greater" (spanOf js)


validator :: DslValidator a -> Validator a
validator p js@(JObject _ m) = do
    (output, expectedKeys) <- runValidator p js
    validateKeys expectedKeys *> pure output
  where
    validateKeys :: [Text] -> Result ()
    validateKeys expectedKeys =
        let actualKeys = Map.keys m
            isUnexpectedKey k = keyLabel k `notElem` expectedKeys
            keyError k = unexpectedKey k (Candidates expectedKeys)
        in traverse keyError (filter isUnexpectedKey actualKeys) *> pure ()

validator _ js =
    typeError (Expected "object") (Actual (typeOf js)) (spanOf js)
