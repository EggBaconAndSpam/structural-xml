{- Wrapper types -}
module Data.XML.XSD where
{-
import Data.Text (Text)
import Data.XML
import Data.XML.Parse.Generically
import GHC.Generics

data XsdSequence = XsdSequence
  { element :: [XsdElement]
  }
  deriving stock (Generic)
  deriving (MapNamesToXML) via MapNamesId XsdSequence
  deriving (FromElement) via GenericOrdered XsdSequence

data IntOrUnbounded = Int Int | Unbounded

instance FromContent IntOrUnbounded where
  fromContent "unbounded" _ = pure Unbounded
  fromContent t i = Int <$> fromContent t i

-- GenericAttributeElement?
data XsdElement = XsdElement
  { name :: Attribute Text,
    typ :: Attribute Text,
    minOccurs :: Attribute (Maybe IntOrUnbounded),
    maxOccurs :: Attribute (Maybe IntOrUnbounded)
  }
  deriving stock (Generic)
  deriving (FromElement) via GenericOrdered XsdElement

instance MapNamesToXML XsdElement where
  mapNamesToXML "typ" = "type"
  mapNamesToXML n = mapNamesToXML @(MapNamesId XsdElement) n

-- simpleContent / sequence / choice
-- GenericChoice
data XsdComplexType = XsdComplexType
  { name :: Attribute Text,
    choice :: Choice XsdComplexTypeChoice
  }

newtype Choice a = Choice a

-- GenericChoice
data XsdComplexTypeChoice
  = SimpleContent XsdSimpleContent
  | Sequence XsdSequence
  | Choice' XsdChoice

data XsdSimpleContent = XsdSimpleContent
  { extension :: XsdExtension
  }

data XsdExtension = XsdExtension
  { base :: Attribute Text,
    attribute :: XsdAttribute
  }

-- GenericAttributeElement
data XsdAttribute = XsdAttribute
  { name :: Attribute Text,
    typ :: Attribute Text,
    use :: Attribute Use
  }

-- GenericOrdered
data XsdChoice = XsdChoice
  { element :: [XsdElement]
  }

data Use = Optional | Required | Prohibited

-- GenericOrdered
data XsdSimpleType = XsdSimpleType
  { name :: Attribute Text,
    restriction :: XsdRestriction
  }

-- GenericUnordered
data XsdRestriction = XsdRestriction
  { base :: Attribute Text,
    minLength :: Maybe (Value Int),
    maxLength :: Maybe (Value Int),
    pattern :: Maybe (Value Text),
    fractionDigits :: Maybe (Value Int),
    totalDigits :: Maybe (Value Int),
    minInclusive :: Maybe (Value Int),
    enumeration :: [Value Text]
  }

-- GenericAttributeElement
newtype Value a = Value {value :: Attribute a}
-}
