{-# LANGUAGE OverloadedStrings #-}
-- |Models representing the apidoc specification.
--
--    http://apidoc.me/bryzek/apidoc-spec/latest
--
module Apidoc.DSL where

import           Apidoc.Json (Json)
import           Data.Map    (Map)
import           Data.Text   (Text)
import           Network.URI (URI)
import           Prelude     hiding (Enum)

newtype TypeName = TypeName Text
  deriving (Show, Eq)

newtype Uri = Uri URI
  deriving (Show, Eq)

data Spec = Spec {
    specApidoc      :: Maybe Apidoc
  , specAttributes  :: Maybe [Attribute]
  , specBaseUrl     :: Maybe Uri
  , specDescription :: Maybe Text
  , specEnums       :: Maybe (Map TypeName Enum)
  , specHeaders     :: Maybe [Header]
  , specImports     :: Maybe [Import]
  , specInfo        :: Maybe Info
  , specModels      :: Maybe (Map TypeName Model)
  , specName        :: Text
  , specNamespace   :: Maybe Text
  , specResources   :: Maybe (Map TypeName Resource)
  , specUnions      :: Maybe (Map TypeName Union)
  } deriving (Show, Eq)

data Apidoc = Apidoc {
    apidocVersion :: Text
  } deriving (Show, Eq)

data Attribute = Attribute {
    attributeName   :: Text
  , attributeValues :: Json
  } deriving (Show, Eq)

data Enum = Enum {
    enumAttributes  :: Maybe [Attribute]
  , enumDeprecation :: Maybe Deprecation
  , enumDescription :: Maybe Text
  , enumPlural      :: Maybe Text
  , enumValues      :: [EnumValue]
  } deriving (Show, Eq)

data EnumValue = EnumValue {
    enumValueAttributes  :: Maybe [Attribute]
  , enumValueDeprecation :: Maybe Deprecation
  , enumValueDescription :: Maybe Text
  , enumValueName        :: Text
  } deriving (Show, Eq)

data Header = Header {
    headerAttributes  :: Maybe [Attribute]
  , headerDefault     :: Maybe Json
  , headerDeprecation :: Maybe Deprecation
  , headerDescription :: Maybe Text
  , headerName        :: Text
  , headerRequired    :: Maybe Bool
  , headerType        :: Text
  } deriving (Show, Eq)

data Import = Import {
    importUri :: Uri
  } deriving (Show, Eq)

data Info = Info {
    infoLicense :: Maybe License
  , infoContact :: Maybe Contact
  } deriving (Show, Eq)

data Model = Model {
    modelAttributes  :: Maybe [Attribute]
  , modelDeprecation :: Maybe Deprecation
  , modelDescription :: Maybe Text
  , modelFields      :: [Field]
  , modelPlural      :: Maybe Text
  } deriving (Show, Eq)

data Resource = Resource {
    resourceAttributes  :: Maybe [Attribute]
  , resourceDeprecation :: Maybe Deprecation
  , resourceDescription :: Maybe Text
  , resourceOperations  :: [Operation]
  , resourcePath        :: Maybe Text
  } deriving (Show, Eq)

data Union = Union {
    unionName          :: TypeName
  , unionPlural        :: Text
  , unionDiscriminator :: Maybe Text
  , unionDescription   :: Maybe Text
  , unionDeprecation   :: Maybe Deprecation
  , unionTypes         :: [UnionType]
  , unionAttributes    :: Maybe [Attribute]
  } deriving (Show, Eq)

data UnionType = UnionType {
    unionTypeType        :: Text
  , unionTypeDescription :: Maybe Text
  , unionTypeDeprecation :: Maybe Deprecation
  , unionTypeAttributes  :: Maybe [Attribute]
  } deriving (Show, Eq)

data Body = Body {
    bodyType        :: Text
  , bodyDescription :: Maybe Text
  , bodyDeprecation :: Maybe Deprecation
  , bodyAttributes  :: Maybe [Attribute]
  } deriving (Show, Eq)

data Contact = Contact {
    contactName  :: Maybe Text
  , contactUrl   :: Maybe Uri
  , contactEmail :: Maybe Text
  } deriving (Show, Eq)

data Deprecation = Deprecation {
    deprecationDescription :: Maybe Text
  } deriving (Show, Eq)

data Field = Field {
    fieldName        :: Text
  , fieldType        :: Text
  , fieldDescription :: Maybe Text
  , fieldDeprecation :: Maybe Deprecation
  , fieldDefault     :: Maybe Json
  , fieldRequired    :: Bool
  , fieldMinimum     :: Maybe Integer
  , fieldMaximum     :: Maybe Integer
  , fieldExample     :: Maybe Text
  , fieldAttributes  :: Maybe [Attribute]
  } deriving (Show, Eq)

data License = License {
    licenseName :: Text
  , licenseUrl  :: Maybe Uri
  } deriving (Show, Eq)

data Operation = Operation {
    operationAttributes  :: Maybe [Attribute]
  , operationBody        :: Maybe Body
  , operationDeprecation :: Maybe Deprecation
  , operationDescription :: Maybe Text
  , operationMethod      :: Method
  , operationParameters  :: Maybe [Parameter]
  , operationPath        :: Maybe Text
  , operationResponses   :: Maybe (Map ResponseCode Response)
  } deriving (Show, Eq)

data Method = GET | POST | PUT | PATCH | DELETE | HEAD | CONNECT | OPTIONS | TRACE
  deriving (Show, Eq)

data ResponseCode = RespInt Integer | RespDefault
  deriving (Show, Eq)

data Response = Response {
    responseDeprecation :: Deprecation
  , responseDescription :: Maybe Text
  , responseType        :: Text
  } deriving (Show, Eq)

data Parameter = Parameter {
    parameterDefault     :: Maybe Json
  , parameterDeprecation :: Maybe Deprecation
  , parameterDescription :: Maybe Text
  , parameterExample     :: Maybe Text
  , parameterLocation    :: ParameterLocation
  , parameterMaximum     :: Maybe Integer
  , parameterMinimum     :: Maybe Integer
  , parameterName        :: Text
  , parameterRequired    :: Maybe Bool
  , parameterType        :: Text
  } deriving (Show, Eq)

data ParameterLocation = Path | Query | Form
  deriving (Show, Eq)
