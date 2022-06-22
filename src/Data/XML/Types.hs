module Data.XML.Types
  ( Document (..),
    Element (..),
    emptyElement,
    Node (..),
    fromXmlConduit,
    toXmlConduit,
  )
where

import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Text.XML (Name)
import qualified Text.XML as XC

data Document = Document
  { rootName :: Name,
    root :: Element
  }
  deriving stock (Show)

-- We move element names one level up to the parent element.
data Element = Element
  { attributes :: Map Name Text,
    children :: [Node]
  }
  deriving stock (Show, Eq)

emptyElement :: Element
emptyElement = Element mempty []

data Node
  = NodeElement Name Element
  | NodeContent Text
  deriving stock (Show, Eq)

{- Conversion from and to xml-conduit -}
fromXmlConduit :: XC.Document -> Document
fromXmlConduit doc =
  Document
    { rootName = XC.elementName $ XC.documentRoot doc,
      root = fromXmlConduitElement $ XC.documentRoot doc
    }
  where
    fromXmlConduitElement :: XC.Element -> Element
    fromXmlConduitElement el =
      Element
        { attributes = XC.elementAttributes el,
          children = mapMaybe fromXmlConduitNode $ XC.elementNodes el
        }

    fromXmlConduitNode :: XC.Node -> Maybe Node
    fromXmlConduitNode (XC.NodeContent t) = Just $ NodeContent t
    fromXmlConduitNode (XC.NodeElement el) =
      Just $ NodeElement (XC.elementName el) (fromXmlConduitElement el)
    fromXmlConduitNode _ = Nothing

toXmlConduit :: Document -> XC.Document
toXmlConduit Document {..} =
  XC.Document
    { documentPrologue = XC.Prologue [] Nothing [],
      documentRoot = toXmlConduitElement rootName root,
      documentEpilogue = []
    }
  where
    toXmlConduitElement :: Name -> Element -> XC.Element
    toXmlConduitElement name Element {..} =
      XC.Element
        { elementName = name,
          elementAttributes = attributes,
          elementNodes = map toXmlConduitNode children
        }

    toXmlConduitNode :: Node -> XC.Node
    toXmlConduitNode (NodeContent t) = XC.NodeContent t
    toXmlConduitNode (NodeElement name el) = XC.NodeElement $ toXmlConduitElement name el
