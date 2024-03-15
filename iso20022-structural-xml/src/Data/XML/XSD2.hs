-- Just enough to parse camt.053.001.11.xsd
{-# LANGUAGE OverloadedLabels #-}

module Data.XML.XSD2 where

import Data.Char (toLower)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.XML hiding (Element)
import Data.XML.Generic
import GHC.Generics (Generic)
import Optics

data XsdNamespace

instance MapNamesToXML XsdNamespace where
  mapNamesToElements l =
    (fromString l)
      { nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
      }
  mapNamesToEnum = \case
    initial : rest -> toLower initial : rest
    y -> y

data SchemaDoc = SchemaDoc {schema :: Schema}
  deriving (Show) via ReadShowXmlDocument SchemaDoc
  deriving stock (Generic, Eq)
  deriving (MapNamesToXML) via XsdNamespace
  deriving (FromDocument, ToDocument) via GenericDocument SchemaDoc

schemaElementNames :: Schema -> [Text]
schemaElementNames schema = concatMap complexTypeNames (schema ^. #complexType)

complexTypeNames :: ComplexType -> [Text]
complexTypeNames = \case
  ComplexTypeContent {simpleContent = sc} -> [sc ^. #extension % #attribute % #attr_name]
  ComplexTypeSequence {sequence = s} -> case s of
    SequenceElements {element = es} -> map (view #attr_name) es
    SequenceAny {} -> []
    SequenceChoice {choice = c} -> c ^.. #element % folded % #attr_name
  ComplexTypeChoice {choice = c} -> c ^.. #element % folded % #attr_name

data Schema = Schema
  { attr_elementFormDefault :: Text,
    attr_targetNamespace :: Text,
    element :: [Element],
    complexType :: [ComplexType],
    simpleType :: [SimpleType]
  }
  deriving stock (Generic, Eq)
  deriving (MapNamesToXML) via XsdNamespace
  deriving (ToElement, FromElement) via GenericUnordered Schema

data Sequence
  = SequenceElements {element :: [Element]}
  | SequenceAny {any :: Maybe Any}
  | SequenceChoice {choice :: Choice} -- used by camt053.02
  deriving stock (Generic, Eq)
  deriving (MapNamesToXML) via XsdNamespace
  deriving (ToElement, FromElement) via GenericOrdered Sequence

data Any = Any
  { attr_namespace :: Text,
    attr_processContents :: Text
  }
  deriving stock (Generic, Eq)
  deriving (MapNamesToXML) via XsdNamespace
  deriving (ToElement, FromElement) via GenericOrdered Any

data IntOrUnbounded = Int Int | Unbounded
  deriving stock (Read, Show, Generic, Eq)
  deriving (MapNamesToXML) via XsdNamespace
  deriving (ToContent, FromContent) via GenericEnum IntOrUnbounded

data Element = Element
  { attr_name :: Text,
    attr_type :: Text,
    attr_minOccurs :: Maybe IntOrUnbounded,
    attr_maxOccurs :: Maybe IntOrUnbounded
  }
  deriving stock (Generic, Eq)
  deriving (MapNamesToXML) via XsdNamespace
  deriving (ToElement, FromElement) via GenericOrdered Element

data ComplexType
  = ComplexTypeContent {attr_name :: Text, simpleContent :: SimpleContent}
  | ComplexTypeSequence {attr_name :: Text, sequence :: Sequence}
  | ComplexTypeChoice {attr_name :: Text, choice :: Choice}
  deriving stock (Generic, Eq)
  deriving (MapNamesToXML) via XsdNamespace
  -- GenericOrdered for sums!
  deriving (ToElement, FromElement) via GenericOrdered ComplexType

data SimpleContent = SimpleContent
  { extension :: Extension
  }
  deriving stock (Generic, Eq)
  deriving (MapNamesToXML) via XsdNamespace
  deriving (ToElement, FromElement) via GenericOrdered SimpleContent

data Extension = Extension
  { attr_base :: Text,
    attribute :: Attribute
  }
  deriving stock (Generic, Eq)
  deriving (MapNamesToXML) via XsdNamespace
  deriving (ToElement, FromElement) via GenericOrdered Extension

data Attribute = Attribute
  { attr_name :: Text,
    attr_type :: Text,
    attr_use :: Use
  }
  deriving stock (Generic, Eq)
  deriving (MapNamesToXML) via XsdNamespace
  deriving (ToElement, FromElement) via GenericOrdered Attribute

data Choice = Choice
  { element :: [Element]
  }
  deriving stock (Generic, Eq)
  deriving (MapNamesToXML) via XsdNamespace
  deriving (ToElement, FromElement) via GenericOrdered Choice

data Use = Optional | Required | Prohibited
  deriving stock (Read, Show, Generic, Eq)
  deriving (MapNamesToXML) via XsdNamespace
  deriving (ToContent, FromContent) via GenericEnum Use

data SimpleType = SimpleType
  { attr_name :: Text,
    restriction :: Restriction
  }
  deriving stock (Generic, Eq)
  deriving (MapNamesToXML) via XsdNamespace
  deriving (ToElement, FromElement) via GenericOrdered SimpleType

data Restriction = Restriction
  { attr_base :: Text,
    minLength :: Maybe (Value Int),
    maxLength :: Maybe (Value Int),
    pattern :: Maybe (Value Text),
    fractionDigits :: Maybe (Value Int),
    totalDigits :: Maybe (Value Int),
    minInclusive :: Maybe (Value Int),
    enumeration :: [Value Text]
  }
  deriving stock (Generic, Eq)
  deriving (MapNamesToXML) via XsdNamespace
  deriving (FromElement, ToElement) via GenericUnordered Restriction

data Value a = Value {attr_value :: a}
  deriving stock (Generic, Eq)
  deriving (MapNamesToXML) via XsdNamespace

deriving via GenericOrdered (Value a) instance (FromContent a) => FromElement (Value a)

deriving via GenericOrdered (Value a) instance (ToContent a) => ToElement (Value a)
