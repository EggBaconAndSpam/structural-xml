{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Data.XML.Generics.Example where

import Control.Exception (throw)
import Data.Text (Text)
import qualified Data.Text.Lazy as Text
import Data.XML
import Data.XML.Generics.NamedSum
import Data.XML.Generics.Options
import Data.XML.Helpers.Generically
import Debug.Trace
import GHC.Generics
import Text.XML (def)
import qualified Text.XML as XC

test :: FromElement a => Text -> IO a
test raw = do
  Document {root} <- either throw (pure . fromXmlConduit) $ XC.parseText def $ Text.fromStrict raw
  traceShowM root
  either throw pure $ fromElement root

{-
<xs:complexType name="GroupHeader81">
<xs:sequence>
<xs:element name="MsgId" type="Max35Text"/>
<xs:element name="CreDtTm" type="ISODateTime"/>
<xs:element maxOccurs="1" minOccurs="0" name="MsgRcpt" type="PartyIdentification135"/>
<xs:element maxOccurs="1" minOccurs="0" name="MsgPgntn" type="Pagination1"/>
<xs:element maxOccurs="1" minOccurs="0" name="OrgnlBizQry" type="OriginalBusinessQuery1"/>
<xs:element maxOccurs="1" minOccurs="0" name="AddtlInf" type="Max500Text"/>
</xs:sequence>
</xs:complexType>

<?>
<MsgId>SomeID42</MsgId>
<CreDtTm>1970-01-01-01:00</CreDtTm>
</?>

-}
data GroupHeader81 = GroupHeader81
  { messageId :: Text,
    creationDateTime :: Text,
    messageRecipient :: Maybe Text,
    messagePagination :: Maybe Text,
    originalBusinessQuery :: Maybe Text,
    additionalInfo :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving (FromElement) via Generically '[ 'NothingElementEncoding 'OmitNothing, 'NameModifier] GroupHeader81

instance NameModifier GroupHeader81 where
  nameModifier = \case
    "messageId" -> "MsgId"
    "creationDateTime" -> "CreDtTm"
    "messageRecipient" -> "MsgRcpt"
    "messagePagination" -> "MsgPgntn"
    "originalBusinessQuery" -> "OrgnlBizQry"
    "additionalInfo" -> "AddtlInf"

{-
    <xs:complexType name="AccountIdentification4Choice">
        <xs:choice>
            <xs:element name="IBAN" type="IBAN2007Identifier"/>
            <xs:element name="Othr" type="GenericAccountIdentification1"/>
        </xs:choice>
    </xs:complexType>
-}
data AccountIdentification4Choice = IBAN Element | Other Element
  deriving stock (Generic, Show)
  deriving (FromElement) via Generically '[ 'NameModifier] AccountIdentification4Choice

instance NameModifier AccountIdentification4Choice where
  nameModifier = \case
    "IBAN" -> "IBAN"
    "Other" -> "Othr"

data Bound = Bound Int | Unbounded
  deriving stock (Show) -- only need to handle 0,1,unbounded

instance FromContent Bound where
  fromContent "unbounded" = pure Unbounded
  fromContent t = Bound <$> fromContent t

data XsdElement = XsdElement
  { name :: Text,
    maxOccurs :: Maybe Bound,
    minOccurs :: Maybe Bound,
    _type :: Text -- `type` is not a legal field name in Haskell
  }
  deriving stock (Generic, Show)
  deriving (FromElement) via Generically '[ 'AttributePrefix "", 'Rename "type_" "type"] XsdElement

-- sequence of elements (record)
newtype XsdSequence = XsdSequence
  { element :: [XsdElement]
  }
  deriving stock (Generic, Show)
  deriving (FromElement) via Generically '[] XsdSequence

data ChoiceElement = ChoiceElement
  { name :: Text,
    _type :: Text -- `type` is not a legal field name in Haskell
  }
  deriving stock (Generic, Show)
  deriving (FromElement) via Generically '[ 'AttributePrefix "", 'Rename "_type" "type"] ChoiceElement

newtype XsdChoice = XsdChoice
  { element :: [ChoiceElement]
  }
  deriving stock (Generic, Show)
  deriving (FromElement) via Generically '[] XsdChoice

data ComplexType = ComplexType
  { attr_name :: Text,
    sum :: NamedSum '[ '("sequence", XsdSequence), '("choice", XsdChoice)]
  }
  deriving stock (Generic, Show)
  deriving (FromElement) via Generically '[ 'SumPrefix "sum", 'AttributePrefix "attr_"] ComplexType

data SimpleType = SimpleType
  { attr_name :: Text,
    restriction :: SimpleTypeRestriction
  }
  deriving stock (Generic, Show)
  deriving (FromElement) via Generically '[ 'AttributePrefix "attr_"] SimpleType

-- need unordered option, corresponding to xsd all!
data SimpleTypeRestriction = SimpleTypeRestriction
  { attr_base :: Text,
    minExcluse :: Maybe Element,
    minInclusive :: Maybe Element,
    maxExclusive :: Maybe Element,
    maxInclusive :: Maybe Element,
    totalDigits :: Maybe Element,
    fractionDigits :: Maybe Element,
    length :: Maybe Element,
    minLength :: Maybe Element,
    maxLength :: Maybe Element,
    enumeration :: [Element],
    whiteSpace :: Maybe Element,
    pattern :: Maybe Element
  }
  deriving stock (Generic, Show)
  deriving (FromElement) via Generically '[ 'AttributePrefix "attr_", 'Unordered] SimpleTypeRestriction
