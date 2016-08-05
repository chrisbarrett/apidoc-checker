module Apidoc.DSL.Types where

import           Apidoc.Json (Json)
import           Data.Map    (Map)
import           Data.Text   (Text)
import           Network.URI (URI)
import           Prelude     hiding (Enum)

data TypeRef = TNominal TypeName
             | TMap TypeName
             | TArray TypeName
  deriving (Show, Eq, Ord)

data BuiltIn = TBoolean
             | TDateIso8601
             | TDateTimeIso8601
             | TDecimal
             | TDouble
             | TInteger
             | TLong
             | TObject
             | TString
             | TUnit
             | TUuid
  deriving (Show, Eq, Ord)

newtype TypeName = TypeName {_typeNameLabel :: Text}
  deriving (Show, Eq, Ord)

newtype FieldName = FieldName {_fieldNameLabel :: Text}
  deriving (Show, Eq, Ord)

newtype Uri = Uri {_uriValue :: URI}
  deriving (Show, Eq)

newtype Namespace = Namespace {_namespaceLabel :: Text}
  deriving (Show, Eq, Ord)

newtype ServiceName = ServiceName {_serviceNameLabel :: Text}
  deriving (Show, Eq, Ord)

data Spec = Spec {
    _specApidoc      :: Maybe Apidoc
  , _specAttributes  :: Maybe [Attribute]
  , _specBaseUrl     :: Maybe Uri
  , _specDescription :: Maybe Text
  , _specEnums       :: Maybe (Map TypeName Enum)
  , _specHeaders     :: Maybe [Header]
  , _specImports     :: Maybe [Import]
  , _specInfo        :: Maybe Info
  , _specModels      :: Maybe (Map TypeName Model)
  , _specName        :: ServiceName
  , _specNamespace   :: Maybe Namespace
  , _specResources   :: Maybe (Map TypeRef Resource)
  , _specUnions      :: Maybe (Map TypeName Union)
  } deriving (Show, Eq)

data Apidoc = Apidoc {
    _apidocVersion :: Text
  } deriving (Show, Eq)

data Attribute = Attribute {
    _attributeName  :: Text
  , _attributeValue :: Json
  } deriving (Show, Eq)

data Enum = Enum {
    _enumAttributes  :: Maybe [Attribute]
  , _enumDeprecation :: Maybe Deprecation
  , _enumDescription :: Maybe Text
  , _enumPlural      :: Maybe TypeName
  , _enumValues      :: [EnumValue]
  } deriving (Show, Eq)

data EnumValue = EnumValue {
    _enumValueAttributes  :: Maybe [Attribute]
  , _enumValueDeprecation :: Maybe Deprecation
  , _enumValueDescription :: Maybe Text
  , _enumValueName        :: TypeName
  } deriving (Show, Eq)

data Header = Header {
    _headerAttributes  :: Maybe [Attribute]
  , _headerDefault     :: Maybe Text
  , _headerDeprecation :: Maybe Deprecation
  , _headerDescription :: Maybe Text
  , _headerName        :: Text
  , _headerRequired    :: Maybe Bool
  , _headerType        :: TypeRef
  } deriving (Show, Eq)

data Import = Import {
    _importUri :: Uri
  } deriving (Show, Eq)

data Info = Info {
    _infoLicense :: Maybe License
  , _infoContact :: Maybe Contact
  } deriving (Show, Eq)

data Model = Model {
    _modelAttributes  :: Maybe [Attribute]
  , _modelDeprecation :: Maybe Deprecation
  , _modelDescription :: Maybe Text
  , _modelFields      :: [Field]
  , _modelPlural      :: Maybe TypeName
  } deriving (Show, Eq)

data Resource = Resource {
    _resourceAttributes  :: Maybe [Attribute]
  , _resourceDeprecation :: Maybe Deprecation
  , _resourceDescription :: Maybe Text
  , _resourceOperations  :: [Operation]
  , _resourcePath        :: Maybe Text
  } deriving (Show, Eq)

data Union = Union {
    _unionAttributes    :: Maybe [Attribute]
  , _unionDeprecation   :: Maybe Deprecation
  , _unionDescription   :: Maybe Text
  , _unionDiscriminator :: Maybe Text
  , _unionPlural        :: Maybe TypeName
  , _unionTypes         :: [UnionType]
  } deriving (Show, Eq)

data UnionType = UnionType {
    _unionTypeAttributes  :: Maybe [Attribute]
  , _unionTypeDeprecation :: Maybe Deprecation
  , _unionTypeDescription :: Maybe Text
  , _unionTypeType        :: TypeRef
  } deriving (Show, Eq)

data Body = Body {
    _bodyAttributes  :: Maybe [Attribute]
  , _bodyDeprecation :: Maybe Deprecation
  , _bodyDescription :: Maybe Text
  , _bodyType        :: TypeRef
  } deriving (Show, Eq)

data Contact = Contact {
    _contactEmail :: Maybe Text
  , _contactName  :: Maybe Text
  , _contactUrl   :: Maybe Uri
  } deriving (Show, Eq)

data Deprecation = Deprecation {
    _deprecationDescription :: Maybe Text
  } deriving (Show, Eq)

data Field = Field {
    _fieldAttributes  :: Maybe [Attribute]
  , _fieldDefault     :: Maybe Json
  , _fieldDeprecation :: Maybe Deprecation
  , _fieldDescription :: Maybe Text
  , _fieldExample     :: Maybe Text
  , _fieldMaximum     :: Maybe Integer
  , _fieldMinimum     :: Maybe Integer
  , _fieldName        :: FieldName
  , _fieldRequired    :: Maybe Bool
  , _fieldType        :: TypeRef
  } deriving (Show, Eq)

data License = License {
    _licenseName :: Text
  , _licenseUrl  :: Maybe Uri
  } deriving (Show, Eq)

data Operation = Operation {
    _operationAttributes  :: Maybe [Attribute]
  , _operationBody        :: Maybe Body
  , _operationDeprecation :: Maybe Deprecation
  , _operationDescription :: Maybe Text
  , _operationMethod      :: HttpMethod
  , _operationParameters  :: Maybe [Parameter]
  , _operationPath        :: Maybe Text
  , _operationResponses   :: Maybe (Map ResponseCode Response)
  } deriving (Show, Eq)

data HttpMethod = GET | POST | PUT | PATCH | DELETE | HEAD | CONNECT | OPTIONS | TRACE
  deriving (Show, Eq, Ord)

data ResponseCode = RespInt Integer | RespDefault
  deriving (Show, Eq, Ord)

data Response = Response {
    _responseDeprecation :: Maybe Deprecation
  , _responseDescription :: Maybe Text
  , _responseType        :: TypeRef
  } deriving (Show, Eq)

data Parameter = Parameter {
    _parameterDefault     :: Maybe Json
  , _parameterDeprecation :: Maybe Deprecation
  , _parameterDescription :: Maybe Text
  , _parameterExample     :: Maybe Text
  , _parameterLocation    :: Maybe ParameterLocation
  , _parameterMaximum     :: Maybe Integer
  , _parameterMinimum     :: Maybe Integer
  , _parameterName        :: Text
  , _parameterRequired    :: Maybe Bool
  , _parameterType        :: TypeRef
  } deriving (Show, Eq)

data ParameterLocation = Path | Query | Form
  deriving (Show, Eq, Ord)
