module Data.XML.Types
  ( Document (..),
    Element (..),
    emptyElement,
    Node (..),
    fromXmlConduit,
    fromXmlConduitElement,
    toXmlConduit,
    toXmlConduitElement,
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
  deriving stock (Show, Eq, Ord)

-- We move element names one level up to the parent element.
data Element = Element
  { attributes :: Map Name Text,
    children :: [Node]
  }
  deriving stock (Show, Eq, Ord)

emptyElement :: Element
emptyElement = Element mempty []

data Node
  = NodeElement Name Element
  | NodeContent Text
  deriving stock (Show, Eq, Ord)

{- Conversion from and to xml-conduit -}
fromXmlConduit :: XC.Document -> Document
fromXmlConduit doc =
  Document
    { rootName = XC.elementName $ XC.documentRoot doc,
      root = fromXmlConduitElement $ XC.documentRoot doc
    }

fromXmlConduitElement :: XC.Element -> Element
fromXmlConduitElement el =
  Element
    { attributes = XC.elementAttributes el,
      children = mapMaybe fromXmlConduitNode $ XC.elementNodes el
    }
  where
    fromXmlConduitNode :: XC.Node -> Maybe Node
    fromXmlConduitNode (XC.NodeContent t) = Just $ NodeContent t
    fromXmlConduitNode (XC.NodeElement e) =
      Just $ NodeElement (XC.elementName e) (fromXmlConduitElement e)
    fromXmlConduitNode _ = Nothing

toXmlConduit :: Document -> XC.Document
toXmlConduit Document {..} =
  XC.Document
    { documentPrologue = XC.Prologue [] Nothing [],
      documentRoot = toXmlConduitElement rootName root,
      documentEpilogue = []
    }

toXmlConduitElement :: Name -> Element -> XC.Element
toXmlConduitElement name Element {..} =
  XC.Element
    { elementName = name,
      elementAttributes = attributes,
      elementNodes = map toXmlConduitNode children
    }
  where
    toXmlConduitNode :: Node -> XC.Node
    toXmlConduitNode (NodeContent t) = XC.NodeContent t
    toXmlConduitNode (NodeElement n el) = XC.NodeElement $ toXmlConduitElement n el
