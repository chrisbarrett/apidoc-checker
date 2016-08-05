{-# LANGUAGE TemplateHaskell #-}
module Apidoc.DSL.Lenses where

import           Apidoc.DSL.Types
import           Control.Lens.TH
import           Prelude          hiding (Enum)

makePrisms ''TypeRef

makePrisms ''BuiltIn

makeLenses ''TypeName
makePrisms ''TypeName

makeLenses ''FieldName
makePrisms ''FieldName

makeLenses ''Uri
makePrisms ''Uri

makeLenses ''Namespace
makePrisms ''Namespace

makeLenses ''ServiceName
makePrisms ''ServiceName

makeLenses ''Spec
makePrisms ''Spec

makeLenses ''Apidoc
makePrisms ''Apidoc

makeLenses ''Attribute
makePrisms ''Attribute

makeLenses ''Enum
makePrisms ''Enum

makeLenses ''EnumValue
makePrisms ''EnumValue

makeLenses ''Header
makePrisms ''Header

makeLenses ''Import
makePrisms ''Import

makeLenses ''Info
makePrisms ''Info

makeLenses ''Model
makePrisms ''Model

makeLenses ''Resource
makePrisms ''Resource

makeLenses ''Union
makePrisms ''Union

makeLenses ''UnionType
makePrisms ''UnionType

makeLenses ''Body
makePrisms ''Body

makeLenses ''Contact
makePrisms ''Contact

makeLenses ''Deprecation
makePrisms ''Deprecation

makeLenses ''Field
makePrisms ''Field

makeLenses ''License
makePrisms ''License

makeLenses ''Operation
makePrisms ''Operation

makePrisms ''HttpMethod

makePrisms ''ResponseCode

makeLenses ''Response
makePrisms ''Response

makeLenses ''Parameter
makePrisms ''Parameter

makePrisms ''ParameterLocation
