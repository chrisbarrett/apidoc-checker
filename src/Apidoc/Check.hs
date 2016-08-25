{-# LANGUAGE OverloadedStrings #-}
-- |Implements verification of apidoc specs.
module Apidoc.Check
  (
    module Exports
  , validate
  ) where

import           Apidoc.Check.DSL
import           Apidoc.Check.Env               as Exports (Env)
import           Apidoc.Check.Err               as Exports
import           Apidoc.Check.Lenses            as Exports
import           Apidoc.DSL.Types
import           Apidoc.Json
import qualified Control.Applicative.Validation as Validation
import           Data.Sequence                  (Seq)
import           Prelude                        hiding (Enum)

validate :: Json -> Either (Seq Err) (Seq Err, Spec)
validate js = Validation.runValidation (spec js)


spec :: Json -> Check Spec
spec = object $
    Spec
      <$> optional "apidoc" apidoc
      <*> optional "attributes" (array attribute)
      <*> optional "base_url" uri
      <*> optional "description" string
      <*> optional "enums" (mapOf (key typeName) enum)
      <*> optional "headers" (array header)
      <*> optional "imports" (array import_)
      <*> optional "info" info
      <*> optional "models" (mapOf (key typeName) model)
      <*> required "name" name
      <*> optional "namespace" namespace
      <*> optional "resources" (mapOf (key typeName) resource)
      <*> optional "unions" (mapOf (key typeName) union)


apidoc :: Json -> Check Apidoc
apidoc = object $
    Apidoc
      <$> required "version" string


attribute :: Json -> Check Attribute
attribute = object $ do
    Attribute
      <$> required "name" string
      <*> required "value" anyJson


enum :: Json -> Check Enum
enum = object $
    Enum
      <$> optional "attributes" (array attribute)
      <*> optional "deprecation" deprecation
      <*> optional "description" string
      <*> optional "plural" string
      <*> required "values" (array enumValue)


enumValue :: Json -> Check EnumValue
enumValue = object $
    EnumValue
      <$> optional "attributes" (array attribute)
      <*> optional "deprecation" deprecation
      <*> optional "description" string
      <*> required "name" typeName


deprecation :: Json -> Check Deprecation
deprecation = object $
    Deprecation
      <$> optional "description" string


header :: Json -> Check Header
header = object $
    Header
      <$> optional "attributes" (array attribute)
      <*> optional "default" string
      <*> optional "deprecation" deprecation
      <*> optional "description" string
      <*> required "name" string
      <*> optional "required" bool
      <*> required "type" typeRef


import_ :: Json -> Check Import
import_ = object $
    Import
      <$> required "uri" uri


info :: Json -> Check Info
info = object $
    Info
      <$> optional "license" license
      <*> optional "contact" contact


license :: Json -> Check License
license = object $
    License
      <$> required "name" string
      <*> optional "url" uri


contact :: Json -> Check Contact
contact = object $
    Contact
      <$> optional "email" string
      <*> optional "name" string
      <*> optional "url" uri


model :: Json -> Check Model
model = object $
    Model
      <$> optional "attributes" (array attribute)
      <*> optional "deprecation" deprecation
      <*> optional "description" string
      <*> required "fields" (array field)
      <*> optional "plural" string


field :: Json -> Check Field
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


resource :: Json -> Check Resource
resource = object $
    Resource
      <$> optional "attributes" (array attribute)
      <*> optional "deprecation" deprecation
      <*> optional "description" string
      <*> required "operations" (array operation)
      <*> optional "path" string


operation :: Json -> Check Operation
operation = object $
    Operation
      <$> optional "attributes" (array attribute)
      <*> optional "body" body
      <*> optional "deprecation" deprecation
      <*> optional "description" string
      <*> required "method" httpMethod
      <*> optional "parameters" (array parameter)
      <*> optional "path" string
      <*> optional "responses" (mapOf (key responseCode) response)


parameter :: Json -> Check Parameter
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


response :: Json -> Check Response
response = object $
    Response
      <$> optional "deprecation" deprecation
      <*> optional "description" string
      <*> required "type" typeRef


body :: Json -> Check Body
body = object $
    Body
      <$> optional "attributes" (array attribute)
      <*> optional "deprecation" deprecation
      <*> optional "description" string
      <*> required "type" typeRef


union :: Json -> Check Union
union = object $
    Union
      <$> optional "attributes" (array attribute)
      <*> optional "deprecation" deprecation
      <*> optional "description" string
      <*> optional "discriminator" string
      <*> optional "plural" string
      <*> required "types" (array unionType)


unionType :: Json -> Check UnionType
unionType = object $
    UnionType
      <$> optional "attributes" (array attribute)
      <*> optional "deprecation" deprecation
      <*> optional "description" string
      <*> required "type" typeRef


name :: Json -> Check ServiceName
name js =
    ServiceName <$> string js


namespace :: Json -> Check Namespace
namespace js =
    Namespace <$> string js

typeName :: Json -> Check TypeName
typeName js =
    TypeName <$> string js
