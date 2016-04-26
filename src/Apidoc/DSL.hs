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

data TypeRef = TSimple SimpleType
             | TMap SimpleType
             | TArray SimpleType
  deriving (Show, Eq, Ord)

data SimpleType = TRemote Namespace TypeName
                | BuiltIn BuiltIn
                | TLocal TypeName
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

newtype TypeName = TypeName {_unTypeName :: Text}
  deriving (Show, Eq, Ord)

newtype FieldName = FieldName Text
  deriving (Show, Eq, Ord)

newtype Uri = Uri URI
  deriving (Show, Eq)

newtype Namespace = Namespace Text
  deriving (Show, Eq, Ord)

newtype ServiceName = ServiceName Text
  deriving (Show, Eq, Ord)

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
  , specName        :: ServiceName
  , specNamespace   :: Maybe Namespace
  , specResources   :: Maybe (Map TypeRef Resource)
  , specUnions      :: Maybe (Map TypeName Union)
  } deriving (Show, Eq)

data Apidoc = Apidoc {
    apidocVersion :: Text
  } deriving (Show, Eq)

data Attribute = Attribute {
    attributeName   :: Text
  , attributeValues :: Json ()
  } deriving (Show, Eq)

data Enum = Enum {
    enumAttributes  :: Maybe [Attribute]
  , enumDeprecation :: Maybe Deprecation
  , enumDescription :: Maybe Text
  , enumPlural      :: Maybe TypeName
  , enumValues      :: [EnumValue]
  } deriving (Show, Eq)

data EnumValue = EnumValue {
    enumValueAttributes  :: Maybe [Attribute]
  , enumValueDeprecation :: Maybe Deprecation
  , enumValueDescription :: Maybe Text
  , enumValueName        :: TypeName
  } deriving (Show, Eq)

data Header = Header {
    headerAttributes  :: Maybe [Attribute]
  , headerDefault     :: Maybe Text
  , headerDeprecation :: Maybe Deprecation
  , headerDescription :: Maybe Text
  , headerName        :: Text
  , headerRequired    :: Maybe Bool
  , headerType        :: TypeRef
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
  , modelPlural      :: Maybe TypeName
  } deriving (Show, Eq)

data Resource = Resource {
    resourceAttributes  :: Maybe [Attribute]
  , resourceDeprecation :: Maybe Deprecation
  , resourceDescription :: Maybe Text
  , resourceOperations  :: [Operation]
  , resourcePath        :: Maybe Text
  } deriving (Show, Eq)

data Union = Union {
    unionAttributes    :: Maybe [Attribute]
  , unionDeprecation   :: Maybe Deprecation
  , unionDescription   :: Maybe Text
  , unionDiscriminator :: Maybe Text
  , unionPlural        :: Maybe TypeName
  , unionTypes         :: [UnionType]
  } deriving (Show, Eq)

data UnionType = UnionType {
    unionTypeAttributes  :: Maybe [Attribute]
  , unionTypeDeprecation :: Maybe Deprecation
  , unionTypeDescription :: Maybe Text
  , unionTypeType        :: TypeRef
  } deriving (Show, Eq)

data Body = Body {
    bodyAttributes  :: Maybe [Attribute]
  , bodyDeprecation :: Maybe Deprecation
  , bodyDescription :: Maybe Text
  , bodyType        :: TypeRef
  } deriving (Show, Eq)

data Contact = Contact {
    contactEmail :: Maybe Text
  , contactName  :: Maybe Text
  , contactUrl   :: Maybe Uri
  } deriving (Show, Eq)

data Deprecation = Deprecation {
    deprecationDescription :: Maybe Text
  } deriving (Show, Eq)

data Field = Field {
    fieldAttributes  :: Maybe [Attribute]
  , fieldDefault     :: Maybe (Json ())
  , fieldDeprecation :: Maybe Deprecation
  , fieldDescription :: Maybe Text
  , fieldExample     :: Maybe Text
  , fieldMaximum     :: Maybe Integer
  , fieldMinimum     :: Maybe Integer
  , fieldName        :: FieldName
  , fieldRequired    :: Maybe Bool
  , fieldType        :: TypeRef
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
  , operationMethod      :: HttpMethod
  , operationParameters  :: Maybe [Parameter]
  , operationPath        :: Maybe Text
  , operationResponses   :: Maybe (Map ResponseCode Response)
  } deriving (Show, Eq)

data HttpMethod = GET | POST | PUT | PATCH | DELETE | HEAD | CONNECT | OPTIONS | TRACE
  deriving (Show, Eq, Ord)

data ResponseCode = RespInt Integer | RespDefault
  deriving (Show, Eq, Ord)

data Response = Response {
    responseDeprecation :: Maybe Deprecation
  , responseDescription :: Maybe Text
  , responseType        :: TypeRef
  } deriving (Show, Eq)

data Parameter = Parameter {
    parameterDefault     :: Maybe (Json ())
  , parameterDeprecation :: Maybe Deprecation
  , parameterDescription :: Maybe Text
  , parameterExample     :: Maybe Text
  , parameterLocation    :: Maybe ParameterLocation
  , parameterMaximum     :: Maybe Integer
  , parameterMinimum     :: Maybe Integer
  , parameterName        :: Text
  , parameterRequired    :: Maybe Bool
  , parameterType        :: TypeRef
  } deriving (Show, Eq)

data ParameterLocation = Path | Query | Form
  deriving (Show, Eq, Ord)
