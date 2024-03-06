-- Just enough to parse camt.053.001.11.xsd
module Data.XML.XSD2 where

import Data.Char (toLower)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.XML hiding (Element)
import Data.XML.Parse.Generic
import GHC.Generics (Generic)

data XsdNamespace

instance MapNamesToXML XsdNamespace where
  mapNamesToElements l =
    (fromString l)
      { nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
      }

data SchemaDoc = SchemaDoc {schema :: Schema}
  deriving (Show) via ReadShowXmlDocument SchemaDoc
  deriving stock (Generic, Eq)
  deriving (MapNamesToXML) via XsdNamespace
  deriving (FromDocument, ToDocument) via GenericDocument SchemaDoc

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

data Sequence = Sequence
  { element :: [Element],
    any :: Maybe Any
  }
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
  deriving (ToContent, FromContent) via GenericEnum IntOrUnbounded

instance MapNamesToXML IntOrUnbounded where
  mapNamesToEnum = \case
    initial : rest -> toLower initial : rest
    y -> y

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
  deriving (ToContent, FromContent) via GenericEnum Use

instance MapNamesToXML Use where
  mapNamesToEnum = \case
    initial : rest -> toLower initial : rest
    y -> y

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

{-
complextype choice elements -> sum type, e.g.
    <xs:complexType name="AccountIdentification4Choice">
        <xs:choice>
            <xs:element name="IBAN" type="IBAN2007Identifier"/>
            <xs:element name="Othr" type="GenericAccountIdentification1"/>
        </xs:choice>
    </xs:complexType>
data AccountIdentification4Choice =
   AccountIdentification4ChoiceIBAN {elem_IBAN :: IBAN2007Identifier}
  | AccountIdentification4ChoiceOthr {elem_Othr :: GenericAccountIdentification1}

SimpleType -> FromContent + FromElement via ContentElement type. e.g.

    <xs:simpleType name="BaseOneRate">
        <xs:restriction base="xs:decimal">
            <xs:fractionDigits value="10"/>
            <xs:totalDigits value="11"/>
        </xs:restriction>
    </xs:simpleType>

newtype BaseOneRate = BaseOneRate {unBaseOneRate :: Decimal}
mkBaseOneRate :: Decimal -> Either String BaseOneRate
mkBaseOneRate d = do
  restrictFractionDigits 10 d
  restrictTotalDigits 11 d
  pure (BaseRate d)

instance FromContent BaseOneRate where
  fromContent t i = either (Left . parserError i) (Right . BaseOneRate) =<< fromcontent t i

    <xs:simpleType name="NamePrefix2Code">
        <xs:restriction base="xs:string">
            <xs:enumeration value="DOCT"/>
            <xs:enumeration value="MADM"/>
            <xs:enumeration value="MISS"/>
            <xs:enumeration value="MIST"/>
            <xs:enumeration value="MIKS"/>
        </xs:restriction>
    </xs:simpleType>

-- GenericContentEnum
data NamePrefix2Code = DOCT | MADM | MISS | MIST | MIKS

    <xs:simpleType name="PhoneNumber">
        <xs:restriction base="xs:string">
            <xs:pattern value="\+[0-9]{1,3}-[0-9()+\-]{1,30}"/>
        </xs:restriction>
    </xs:simpleType>

newtype PhoneNumber = PhoneNumber {unPhoneNumber :: Text}

mkPhoneNumber :: Text -> Either String PhoneNumber
mkPhoneNumber t = do
  restrictPattern "\+[0-9]{1,3}-[0-9()+\-]{1,30}" t
  pure (PhoneNumber t)

    <xs:simpleType name="YesNoIndicator">
        <xs:restriction base="xs:boolean"/>
    </xs:simpleType>

newtype YesNoIndicator = YesNoIndicator Bool
  deriving newtype (FromContent, ToContent)

    <xs:complexType name="RemittanceAmount3">
        <xs:sequence>
            <xs:element maxOccurs="1" minOccurs="0" name="DuePyblAmt" type="ActiveOrHistoricCurrencyAndAmount"/>
            <xs:element maxOccurs="unbounded" minOccurs="0" name="DscntApldAmt" type="DiscountAmountAndType1"/>
            <xs:element maxOccurs="1" minOccurs="0" name="CdtNoteAmt" type="ActiveOrHistoricCurrencyAndAmount"/>
            <xs:element maxOccurs="unbounded" minOccurs="0" name="TaxAmt" type="TaxAmountAndType1"/>
            <xs:element maxOccurs="unbounded" minOccurs="0" name="AdjstmntAmtAndRsn" type="DocumentAdjustment1"/>
            <xs:element maxOccurs="1" minOccurs="0" name="RmtdAmt" type="ActiveOrHistoricCurrencyAndAmount"/>
        </xs:sequence>
    </xs:complexType>

data RemittanceAmount3 = RemittanceAmount3 {
    duePyblAmt :: Maybe ActiveOrHistoricCurrencyAndAmount,
    dscntApldAmt :: [DiscountAmountAndType1],
    cdtNoteAmt :: Maybe ActiveOrHistoricCurrencyAndAmount,
    taxAmt :: [TaxAmountAndType1],
    adjstmntAmtAndRsn :: [DocumentAdjustment1],
    rmtdAmt :: Maybe ActiveOrHistoricCurrencyAndAmount
    }

    <xs:complexType name="ReportingSource1Choice">
        <xs:choice>
            <xs:element name="Cd" type="ExternalReportingSource1Code"/>
            <xs:element name="Prtry" type="Max35Text"/>
        </xs:choice>
    </xs:complexType>

data ReportingSource1Choice =
  ReportingSource1ChoiceCd { cd :: ExternalReportingSource1Code }
  | ReportingSource1ChoicePrtry { prtry :: Max35Text }

-}
