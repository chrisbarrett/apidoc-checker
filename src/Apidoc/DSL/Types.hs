module Apidoc.DSL.Types where

import           Apidoc.Json   (Json)
import           Data.Map      (Map)
import           Data.Sequence (Seq)
import           Data.Text     (Text)
import           Network.URI   (URI)
import           Prelude       hiding (Enum)

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

newtype Uri = Uri {_uriValue :: URI}
  deriving (Show, Eq)

newtype Namespace = Namespace {_namespaceLabel :: Text}
  deriving (Show, Eq, Ord)

newtype ServiceName = ServiceName {_serviceNameLabel :: Text}
  deriving (Show, Eq, Ord)

data Spec = Spec {
    _specApidoc      :: Maybe Apidoc
  , _specAttributes  :: Maybe (Seq Attribute)
  , _specBaseUrl     :: Maybe Uri
  , _specDescription :: Maybe Text
  , _specEnums       :: Maybe (Map TypeName Enum)
  , _specHeaders     :: Maybe (Seq Header)
  , _specImports     :: Maybe (Seq Import)
  , _specInfo        :: Maybe Info
  , _specModels      :: Maybe (Map TypeName Model)
  , _specName        :: ServiceName
  , _specNamespace   :: Maybe Namespace
  , _specResources   :: Maybe (Map TypeName Resource)
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
    _enumAttributes  :: Maybe (Seq Attribute)
  , _enumDeprecation :: Maybe Deprecation
  , _enumDescription :: Maybe Text
  , _enumPlural      :: Maybe Text
  , _enumValues      :: Seq EnumValue
  } deriving (Show, Eq)

data EnumValue = EnumValue {
    _enumValueAttributes  :: Maybe (Seq Attribute)
  , _enumValueDeprecation :: Maybe Deprecation
  , _enumValueDescription :: Maybe Text
  , _enumValueName        :: TypeName
  } deriving (Show, Eq)

data Header = Header {
    _headerAttributes  :: Maybe (Seq Attribute)
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
    _modelAttributes  :: Maybe (Seq Attribute)
  , _modelDeprecation :: Maybe Deprecation
  , _modelDescription :: Maybe Text
  , _modelFields      :: Seq Field
  , _modelPlural      :: Maybe Text
  } deriving (Show, Eq)

data Resource = Resource {
    _resourceAttributes  :: Maybe (Seq Attribute)
  , _resourceDeprecation :: Maybe Deprecation
  , _resourceDescription :: Maybe Text
  , _resourceOperations  :: Seq Operation
  , _resourcePath        :: Maybe Text
  } deriving (Show, Eq)

data Union = Union {
    _unionAttributes    :: Maybe (Seq Attribute)
  , _unionDeprecation   :: Maybe Deprecation
  , _unionDescription   :: Maybe Text
  , _unionDiscriminator :: Maybe Text
  , _unionPlural        :: Maybe Text
  , _unionTypes         :: Seq UnionType
  } deriving (Show, Eq)

data UnionType = UnionType {
    _unionTypeAttributes  :: Maybe (Seq Attribute)
  , _unionTypeDeprecation :: Maybe Deprecation
  , _unionTypeDescription :: Maybe Text
  , _unionTypeType        :: TypeRef
  } deriving (Show, Eq)

data Body = Body {
    _bodyAttributes  :: Maybe (Seq Attribute)
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
    _fieldAttributes  :: Maybe (Seq Attribute)
  , _fieldDefault     :: Maybe Json
  , _fieldDeprecation :: Maybe Deprecation
  , _fieldDescription :: Maybe Text
  , _fieldExample     :: Maybe Text
  , _fieldMaximum     :: Maybe Integer
  , _fieldMinimum     :: Maybe Integer
  , _fieldName        :: Text
  , _fieldRequired    :: Maybe Bool
  , _fieldType        :: TypeRef
  } deriving (Show, Eq)

data License = License {
    _licenseName :: Text
  , _licenseUrl  :: Maybe Uri
  } deriving (Show, Eq)

data Operation = Operation {
    _operationAttributes  :: Maybe (Seq Attribute)
  , _operationBody        :: Maybe Body
  , _operationDeprecation :: Maybe Deprecation
  , _operationDescription :: Maybe Text
  , _operationMethod      :: HttpMethod
  , _operationParameters  :: Maybe (Seq Parameter)
  , _operationPath        :: Maybe Text
  , _operationResponses   :: Maybe (Map ResponseCode Response)
  } deriving (Show, Eq)

data HttpMethod = GET | POST | PUT | PATCH | DELETE | HEAD | CONNECT | OPTIONS | TRACE
  deriving (Show, Eq, Ord, Read)

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
  deriving (Eq, Ord, Show)
