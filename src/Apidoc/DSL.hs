{-# LANGUAGE OverloadedStrings #-}
-- |Models representing the apidoc specification.
--
--    http://apidoc.me/bryzek/apidoc-spec/latest
--
module Apidoc.DSL where

import           Apidoc.Json         (Json)
import           Data.Map            (Map)
import           Data.Text           (Text)
import           Network.URI         (URI)
import           Prelude             hiding (Enum)
import           Text.Trifecta.Delta (Delta)

newtype TypeName = TypeName Text
  deriving (Show, Eq)

newtype Uri = Uri URI
  deriving (Show, Eq)

data Spec = Spec {
    specApidoc      :: Maybe (Apidoc, Delta)
  , specAttributes  :: Maybe [(Attribute, Delta)]
  , specBaseUrl     :: Maybe (Uri, Delta)
  , specDescription :: Maybe (Text, Delta)
  , specEnums       :: Maybe (Map (TypeName, Delta) (Enum, Delta))
  , specHeaders     :: Maybe [(Header, Delta)]
  , specImports     :: Maybe [(Import, Delta)]
  , specInfo        :: Maybe (Info, Delta)
  , specModels      :: Maybe (Map (TypeName, Delta) (Model, Delta))
  , specName        :: (Text, Delta)
  , specNamespace   :: Maybe (Text, Delta)
  , specResources   :: Maybe (Map (TypeName, Delta) (Resource, Delta))
  , specUnions      :: Maybe (Map (TypeName, Delta) (Union, Delta))
  } deriving (Show, Eq)

data Apidoc = Apidoc {
    apidocVersion :: (Text, Delta)
  } deriving (Show, Eq)

data Attribute = Attribute {
    attributeName   :: (Text, Delta)
  , attributeValues :: (Json, Delta)
  } deriving (Show, Eq)

data Enum = Enum {
    enumAttributes  :: Maybe [(Attribute, Delta)]
  , enumDeprecation :: Maybe (Deprecation, Delta)
  , enumDescription :: Maybe (Text, Delta)
  , enumPlural      :: Maybe (Text, Delta)
  , enumValues      :: [(EnumValue, Delta)]
  } deriving (Show, Eq)

data EnumValue = EnumValue {
    enumValueAttributes  :: Maybe [(Attribute, Delta)]
  , enumValueDeprecation :: Maybe (Deprecation, Delta)
  , enumValueDescription :: Maybe (Text, Delta)
  , enumValueName        :: (Text, Delta)
  } deriving (Show, Eq)

data Header = Header {
    headerAttributes  :: Maybe [(Attribute, Delta)]
  , headerDefault     :: Maybe (Json, Delta)
  , headerDeprecation :: Maybe (Deprecation, Delta)
  , headerDescription :: Maybe (Text, Delta)
  , headerName        :: (Text, Delta)
  , headerRequired    :: Maybe (Bool, Delta)
  , headerType        :: (Text, Delta)
  } deriving (Show, Eq)

data Import = Import {
    importUri :: (Uri, Delta)
  } deriving (Show, Eq)

data Info = Info {
    infoLicense :: Maybe (License, Delta)
  , infoContact :: Maybe (Contact, Delta)
  } deriving (Show, Eq)

data Model = Model {
    modelAttributes  :: Maybe [(Attribute, Delta)]
  , modelDeprecation :: Maybe (Deprecation, Delta)
  , modelDescription :: Maybe (Text, Delta)
  , modelFields      :: [(Field, Delta)]
  , modelPlural      :: Maybe (Text, Delta)
  } deriving (Show, Eq)

data Resource = Resource {
    resourceAttributes  :: Maybe [(Attribute, Delta)]
  , resourceDeprecation :: Maybe (Deprecation, Delta)
  , resourceDescription :: Maybe (Text, Delta)
  , resourceOperations  :: [(Operation, Delta)]
  , resourcePath        :: Maybe (Text, Delta)
  } deriving (Show, Eq)

data Union = Union {
    unionName          :: (TypeName, Delta)
  , unionPlural        :: (Text, Delta)
  , unionDiscriminator :: Maybe (Text, Delta)
  , unionDescription   :: Maybe (Text, Delta)
  , unionDeprecation   :: Maybe (Deprecation, Delta)
  , unionTypes         :: [(UnionType, Delta)]
  , unionAttributes    :: Maybe [(Attribute, Delta)]
  } deriving (Show, Eq)

data UnionType = UnionType {
    unionTypeType        :: (Text, Delta)
  , unionTypeDescription :: Maybe (Text, Delta)
  , unionTypeDeprecation :: Maybe (Deprecation, Delta)
  , unionTypeAttributes  :: Maybe [(Attribute, Delta)]
  } deriving (Show, Eq)

data Body = Body {
    bodyType        :: (Text, Delta)
  , bodyDescription :: Maybe (Text, Delta)
  , bodyDeprecation :: Maybe (Deprecation, Delta)
  , bodyAttributes  :: Maybe [(Attribute, Delta)]
  } deriving (Show, Eq)

data Contact = Contact {
    contactName  :: Maybe (Text, Delta)
  , contactUrl   :: Maybe (Uri, Delta)
  , contactEmail :: Maybe (Text, Delta)
  } deriving (Show, Eq)

data Deprecation = Deprecation {
    deprecationDescription :: Maybe (Text, Delta)
  } deriving (Show, Eq)

data Field = Field {
    fieldName        :: (Text, Delta)
  , fieldType        :: (Text, Delta)
  , fieldDescription :: Maybe (Text, Delta)
  , fieldDeprecation :: Maybe (Deprecation, Delta)
  , fieldDefault     :: Maybe (Json, Delta)
  , fieldRequired    :: (Bool, Delta)
  , fieldMinimum     :: Maybe (Integer, Delta)
  , fieldMaximum     :: Maybe (Integer, Delta)
  , fieldExample     :: Maybe (Text, Delta)
  , fieldAttributes  :: Maybe [(Attribute, Delta)]
  } deriving (Show, Eq)

data License = License {
    licenseName :: (Text, Delta)
  , licenseUrl  :: Maybe (Uri, Delta)
  } deriving (Show, Eq)

data Operation = Operation {
    operationAttributes  :: Maybe [(Attribute, Delta)]
  , operationBody        :: Maybe (Body, Delta)
  , operationDeprecation :: Maybe (Deprecation, Delta)
  , operationDescription :: Maybe (Text, Delta)
  , operationMethod      :: (Method, Delta)
  , operationParameters  :: Maybe [(Parameter, Delta)]
  , operationPath        :: Maybe (Text, Delta)
  , operationResponses   :: Maybe (Map (ResponseCode, Delta) (Response, Delta))
  } deriving (Show, Eq)

data Method = GET | POST | PUT | PATCH | DELETE | HEAD | CONNECT | OPTIONS | TRACE
  deriving (Show, Eq)

data ResponseCode = RespInt Integer | RespDefault
  deriving (Show, Eq)

data Response = Response {
    responseDeprecation :: (Deprecation, Delta)
  , responseDescription :: Maybe (Text, Delta)
  , responseType        :: (Text, Delta)
  } deriving (Show, Eq)

data Parameter = Parameter {
    parameterDefault     :: Maybe (Json, Delta)
  , parameterDeprecation :: Maybe (Deprecation, Delta)
  , parameterDescription :: Maybe (Text, Delta)
  , parameterExample     :: Maybe (Text, Delta)
  , parameterLocation    :: (ParameterLocation, Delta)
  , parameterMaximum     :: Maybe (Integer, Delta)
  , parameterMinimum     :: Maybe (Integer, Delta)
  , parameterName        :: (Text, Delta)
  , parameterRequired    :: Maybe (Bool, Delta)
  , parameterType        :: (Text, Delta)
  } deriving (Show, Eq)

data ParameterLocation = Path | Query | Form
  deriving (Show, Eq)
