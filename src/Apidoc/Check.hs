{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- |Implements verification of apidoc specs.
module Apidoc.Check
  (
    module Exports
  , validate
  ) where

import           Apidoc.Check.CheckM   (Check)
import           Apidoc.Check.Env      as Exports (Env)
import           Apidoc.Check.Err      as Exports (Err (..))
import           Apidoc.Check.Internal
import           Apidoc.Check.Lenses   as Exports
import           Apidoc.DSL.Lenses
import           Apidoc.DSL.Types
import           Apidoc.Json
import           Control.Lens          hiding (enum)
import           Control.Monad
import           Data.Map              (Map)
import qualified Data.Map.Strict       as Map
import           Data.Sequence         (Seq)
import qualified Data.Set              as Set
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Prelude               hiding (Enum, enum)

validate :: MonadPlus m => Env -> Json -> (Seq Err, m Spec)
validate env js = do
    -- (r, s) <- runCheckM (spec js) env
    -- (s ^. envErrs, r)
    undefined


spec :: Check m => Json -> m Spec
spec = object $
    Spec
      <$> optional "apidoc" apidoc
      <*> optional "attributes" (array attribute)
      <*> optional "base_url" uri
      <*> optional "description" string
      <*> optional "enums" (jmap (key typeName) enum)
      <*> optional "headers" (array header)
      <*> optional "imports" (array import_)
      <*> optional "info" info
      <*> optional "models" (jmap (key typeName) model)
      <*> required "name" name
      <*> optional "namespace" namespace
      <*> optional "resources" (jmap (key typeName) resource)
      <*> optional "unions" (jmap (key typeName) union)


apidoc :: Check m => Json -> m Apidoc
apidoc = object $ do
    Apidoc
      <$> required "version" string


attribute :: Check m => Json -> m Attribute
attribute = object $ do
    Attribute
      <$> required "name" string
      <*> required "value" anyJson


enum :: Check m => Json -> m Enum
enum = object $
    Enum
      <$> optional "attributes" (array attribute)
      <*> optional "deprecation" deprecation
      <*> optional "description" string
      <*> optional "plural" string
      <*> required "values" (array enumValue)


enumValue :: Check m => Json -> m EnumValue
enumValue = object $
    EnumValue
      <$> optional "attributes" (array attribute)
      <*> optional "deprecation" deprecation
      <*> optional "description" string
      <*> required "name" typeName


deprecation :: Check m => Json -> m Deprecation
deprecation = object $
    Deprecation
      <$> optional "description" string


header :: Check m => Json -> m Header
header = object $
    Header
      <$> optional "attributes" (array attribute)
      <*> optional "default" string
      <*> optional "deprecation" deprecation
      <*> optional "description" string
      <*> required "name" string
      <*> optional "required" bool
      <*> required "type" typeRef


import_ :: Check m => Json -> m Import
import_ = object $
    Import
      <$> required "uri" uri


info :: Check m => Json -> m Info
info = object $
    Info
      <$> optional "license" license
      <*> optional "contact" contact


license :: Check m => Json -> m License
license = object $
    License
      <$> required "name" string
      <*> optional "url" uri


contact :: Check m => Json -> m Contact
contact = object $
    Contact
      <$> optional "email" string
      <*> optional "name" string
      <*> optional "url" uri


model :: Check m => Json -> m Model
model = object $
    Model
      <$> optional "attributes" (array attribute)
      <*> optional "deprecation" deprecation
      <*> optional "description" string
      <*> required "fields" (array field)
      <*> optional "plural" string


field :: Check m => Json -> m Field
field = object $
    Field
      <$> optional "attributes" (array attribute)
      <*> optional "default" anyJson
      <*> optional "deprecation" deprecation
      <*> optional "description" string
      <*> optional "example" string
      <*> optional "maximum" int
      <*> optional "minimum" int
      <*> required "name" string
      <*> optional "required" bool
      <*> required "type" typeRef


resource :: Check m => Json -> m Resource
resource = object $
    Resource
      <$> optional "attributes" (array attribute)
      <*> optional "deprecation" deprecation
      <*> optional "description" string
      <*> required "operations" (array operation)
      <*> optional "path" string


operation :: Check m => Json -> m Operation
operation = object $
    Operation
      <$> optional "attributes" (array attribute)
      <*> optional "body" body
      <*> optional "deprecation" deprecation
      <*> optional "description" string
      <*> required "method" httpMethod
      <*> optional "parameters" (array parameter)
      <*> optional "path" string
      <*> optional "responses" (jmap (key responseCode) response)


parameter :: Check m => Json -> m Parameter
parameter = object $
    Parameter
      <$> optional "default" anyJson
      <*> optional "deprecation" deprecation
      <*> optional "description" string
      <*> optional "example" string
      <*> optional "location" paramLocation
      <*> optional "minimum" int
      <*> optional "maximum" int
      <*> required "name" string
      <*> optional "required" bool
      <*> required "type" typeRef


response :: Check m => Json -> m Response
response = object $
    Response
      <$> optional "deprecation" deprecation
      <*> optional "description" string
      <*> required "type" typeRef


body :: Check m => Json -> m Body
body = object $
    Body
      <$> optional "attributes" (array attribute)
      <*> optional "deprecation" deprecation
      <*> optional "description" string
      <*> required "type" typeRef


union :: Check m => Json -> m Union
union = object $
    Union
      <$> optional "attributes" (array attribute)
      <*> optional "deprecation" deprecation
      <*> optional "description" string
      <*> optional "discriminator" string
      <*> optional "plural" string
      <*> required "types" (array unionType)


unionType :: Check m => Json -> m UnionType
unionType = object $
    UnionType
      <$> optional "attributes" (array attribute)
      <*> optional "deprecation" deprecation
      <*> optional "description" string
      <*> required "type" typeRef


name :: Check m => Json -> m ServiceName
name js =
    ServiceName <$> string js


namespace :: Check m => Json -> m Namespace
namespace js =
    Namespace <$> string js

typeName :: Check m => Json -> m TypeName
typeName js =
    TypeName <$> string js
