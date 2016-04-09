{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- |Models representing the apidoc specification.
--
--    http://apidoc.me/bryzek/apidoc-spec/latest
--
module Apidoc.Spec where

import           Apidoc.Uri       (Uri)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text        (Text)
import           GHC.Generics     (Generic, Rep)
import           Prelude          hiding (Enum)

-- |JSON parser that accepts objects without record field prefixes.
parseDroppingPrefix :: (Generic a, GFromJSON (Rep a)) => String -> Value -> Parser a
parseDroppingPrefix prefix =
    let opts = defaultOptions {fieldLabelModifier = drop (length prefix)}
    in genericParseJSON opts

data Service = Service {
    serviceApidoc       :: Apidoc
  , serviceName         :: Text
  , serviceOrganization :: Organization
  , serviceApplication  :: Application
  , serviceNamespace    :: Text
  , serviceVersion      :: Text
  , serviceBaseUrl      :: Maybe Uri
  , serviceDescription  :: Maybe Text
  , serviceInfo         :: Info
  , serviceHeaders      :: Maybe [Header]
  , serviceImports      :: Maybe [Import]
  , serviceEnums        :: Maybe [Enum]
  , serviceUnions       :: Maybe [Union]
  , serviceModels       :: Maybe [Model]
  , serviceResources    :: Maybe [Resource]
  , serviceAttributes   :: Maybe [Attribute]
  } deriving (Generic, Show, Eq)

instance FromJSON Service where
    parseJSON = parseDroppingPrefix "Service"

data Method
    = GET
    | POST
    | PUT
    | PATCH
    | DELETE
    | HEAD
    | CONNECT
    | OPTIONS
    | TRACE
  deriving (Generic, Show, Eq, FromJSON)

data ParameterLocation
    = Path
    | Query
    | Form
  deriving (Generic, Show, Eq, FromJSON)

data ResponseCodeOption = Default
  deriving (Generic, Show, Eq, FromJSON)

data Apidoc = Apidoc {
    apidocVersion :: Text
  } deriving (Generic, Show, Eq)

instance FromJSON Apidoc where
    parseJSON = parseDroppingPrefix "apidoc"

data Application = Application {
    applicationKey :: Text
  } deriving (Generic, Show, Eq)

instance FromJSON Application where
    parseJSON = parseDroppingPrefix "application"

data Attribute = Attribute {
    attributeName        :: Text
  , attributeValues      :: Object
  , attributeDescription :: Maybe Text
  , attributeDeprecation :: Maybe Deprecation
  } deriving (Generic, Show, Eq)

instance FromJSON Attribute where
    parseJSON = parseDroppingPrefix "attribute"

data Body = Body {
    bodyType        :: Text
  , bodyDescription :: Maybe Text
  , bodyDeprecation :: Maybe Deprecation
  , bodyAttributes  :: Maybe [Attribute]
  } deriving (Generic, Show, Eq)

instance FromJSON Body where
    parseJSON = parseDroppingPrefix "body"

data Contact = Contact {
    contactName  :: Maybe Text
  , contactUrl   :: Maybe Uri
  , contactEmail :: Maybe Text
  } deriving (Generic, Show, Eq)

instance FromJSON Contact where
    parseJSON = parseDroppingPrefix "contact"

data Deprecation = Deprecation {
    deprecationDescription :: Maybe Text
  } deriving (Generic, Show, Eq)

instance FromJSON Deprecation where
    parseJSON = parseDroppingPrefix "deprecation"

data Enum = Enum {
    enumName        :: Text
  , enumPlural      :: Text
  , enumDescription :: Maybe Text
  , enumDeprecation :: Maybe Deprecation
  , enumValues      :: [EnumValue]
  , enumAttributes  :: Maybe [Attribute]
  } deriving (Generic, Show, Eq)

instance FromJSON Enum where
    parseJSON = parseDroppingPrefix "enum"

data EnumValue = EnumValue {
    enumValueName        :: Text
  , enumValueDescription :: Maybe Text
  , enumValueDeprecation :: Maybe Deprecation
  , enumValueAttributes  :: Maybe [Attribute]
  } deriving (Generic, Show, Eq)

instance FromJSON EnumValue where
    parseJSON = parseDroppingPrefix "enumValue"

data Field = Field {
    fieldName        :: Text
  , fieldType        :: Text
  , fieldDescription :: Maybe Text
  , fieldDeprecation :: Maybe Deprecation
  , fieldDefault     :: Maybe Text
  , fieldRequired    :: Bool
  , fieldMinimum     :: Maybe Integer
  , fieldMaximum     :: Maybe Integer
  , fieldExample     :: Maybe Text
  , fieldAttributes  :: Maybe [Attribute]
  } deriving (Generic, Show, Eq)

instance FromJSON Field where
    parseJSON = parseDroppingPrefix "field"

data Header = Header {
    headerName        :: Text
  , headerType        :: Text
  , headerDescription :: Maybe Text
  , headerDeprecation :: Maybe Deprecation
  , headerRequired    :: Bool
  , headerDefault     :: Maybe Text
  , headerAttributes  :: Maybe [Attribute]
  } deriving (Generic, Show, Eq)

instance FromJSON Header where
    parseJSON = parseDroppingPrefix "header"

data Import = Import {
    importUri          :: Uri
  , importNamespace    :: Text
  , importOrganization :: Organization
  , importApplication  :: Application
  , importVersion      :: Text
  , importEnums        :: Maybe [Text]
  , importUnion        :: Maybe [Text]
  , importModels       :: Maybe [Text]
  } deriving (Generic, Show, Eq)

instance FromJSON Import where
    parseJSON = parseDroppingPrefix "import"

data Info = Info {
    infoLicense :: Maybe License
  , infoContact :: Maybe Contact
  } deriving (Generic, Show, Eq)

instance FromJSON Info where
    parseJSON = parseDroppingPrefix "info"

data License = License {
    licenseName :: Text
  , licenseUrl  :: Maybe Uri
  } deriving (Generic, Show, Eq)

instance FromJSON License where
    parseJSON = parseDroppingPrefix "license"

data Model = Model {
    modelName        :: Text
  , modelPlural      :: Text
  , modelDescription :: Maybe Text
  , modelDeprecation :: Maybe Deprecation
  , modelFields      :: [Field]
  , modelAttributes  :: Maybe [Attribute]
  } deriving (Generic, Show, Eq)

instance FromJSON Model where
    parseJSON = parseDroppingPrefix "model"

data Operation = Operation {
    operationMethod      :: Method
  , operationPath        :: Text
  , operationDescription :: Maybe Text
  , operationDeprecation :: Maybe Deprecation
  , operationBody        :: Maybe Body
  , operationParameters  :: Maybe [Parameter]
  , operationResponses   :: Maybe [Response]
  , operationAttributes  :: Maybe [Attribute]
  } deriving (Generic, Show, Eq)

instance FromJSON Operation where
    parseJSON = parseDroppingPrefix "operation"

data Organization = Organization {
    organizationKey :: Text
  } deriving (Generic, Show, Eq)

instance FromJSON Organization where
    parseJSON = parseDroppingPrefix "organization"

data Parameter = Parameter {
    parameterName        :: Text
  , parameterType        :: Text
  , parameterLocation    :: ParameterLocation
  , parameterDescription :: Maybe Text
  , parameterDeprecation :: Maybe Deprecation
  , parameterRequired    :: Bool
  , parameterDefault     :: Maybe String
  , parameterMinimum     :: Maybe Integer
  , parameterMaximum     :: Maybe Integer
  , parameterExample     :: Maybe Text
  } deriving (Generic, Show, Eq)

instance FromJSON Parameter where
    parseJSON = parseDroppingPrefix "parameter"

data Resource = Resource {
    resourceType        :: Text
  , resourcePlural      :: Text
  , resourcePath        :: Maybe Text
  , resourceDescription :: Maybe Text
  , resourceDeprecation :: Maybe Deprecation
  , resourceOperations  :: Maybe [Operation]
  , resourceAttributes  :: Maybe [Attribute]
  } deriving (Generic, Show, Eq)

instance FromJSON Resource where
    parseJSON = parseDroppingPrefix "resource"

data Response = Response {
    responseCode        :: ResponseCode
  , responseType        :: Text
  , responseDescription :: Maybe Text
  , responseDeprecation :: Deprecation
  } deriving (Generic, Show, Eq)

instance FromJSON Response where
    parseJSON = parseDroppingPrefix "response"

data Union = Union {
    unionName          :: Text
  , unionPlural        :: Text
  , unionDiscriminator :: Maybe Text
  , unionDescription   :: Maybe Text
  , unionDeprecation   :: Maybe Deprecation
  , unionTypes         :: [UnionType]
  , unionAttributes    :: Maybe [Attribute]
  } deriving (Generic, Show, Eq)

instance FromJSON Union where
    parseJSON = parseDroppingPrefix "union"

data UnionType = UnionType {
    unionTypeType        :: Text
  , unionTypeDescription :: Maybe Text
  , unionTypeDeprecation :: Maybe Deprecation
  , unionTypeAttributes  :: Maybe [Attribute]
  } deriving (Generic, Show, Eq)

instance FromJSON UnionType where
    parseJSON = parseDroppingPrefix "unionType"

data ResponseCode = RespInt Integer
                  | RespOpt ResponseCodeOption
  deriving (Generic, Show, Eq, FromJSON)
