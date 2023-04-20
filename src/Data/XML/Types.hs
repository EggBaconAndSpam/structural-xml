module Data.XML.Types
  ( Document (..),
    Element (..),
    emptyElement,
    Node (..),
    fromXmlConduit,
    fromXmlConduitElement,
    toXmlConduit,
    toXmlConduitElement,
    renderName,
    ContentElement (..),
    stripAllWhitespaceContent,
    stripWhitespaceContent,
  )
where

import Data.Char (isSpace)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics
import Text.XML (Name)
import qualified Text.XML as XC

renderName :: Name -> String
renderName XC.Name {nameLocalName, nameNamespace} =
  maybe "" (\ns -> "{" <> Text.unpack ns <> "}") nameNamespace
    <> Text.unpack nameLocalName

data Document = Document
  { rootName :: Name,
    root :: Element
  }
  deriving stock (Show, Eq, Ord, Generic)

-- We move element names one level up to the parent element.
data Element = Element
  { attributes :: Map Name Text,
    children :: [Node]
  }
  deriving stock (Show, Eq, Ord, Generic)

data Node
  = NodeElement Name Element
  | NodeContent Text
  deriving stock (Show, Eq, Ord, Generic)

-- | Helper for elements that only contain content. Has To/FromElement instances
-- defined in terms of To/FromContent.
newtype ContentElement a = ContentElement {content :: a}
  deriving stock (Show, Eq, Ord, Generic)

instance Semigroup Element where
  el <> el' =
    Element
      { attributes = attributes el <> attributes el',
        children = children el <> children el'
      }

instance Monoid Element where
  mempty = emptyElement

emptyElement :: Element
emptyElement = Element mempty []

{- Conversion from and to xml-conduit -}

-- | Strips whitespace content since we don't usually care about that. If you
-- want a lossless conversion instead use fromXmlConduitKeepWhitespaceElements.
fromXmlConduit :: XC.Document -> Document
fromXmlConduit = stripAllWhitespaceContent . fromXmlConduitKeepWhitespaceElements

fromXmlConduitKeepWhitespaceElements :: XC.Document -> Document
fromXmlConduitKeepWhitespaceElements doc =
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

-- | Most of the time we want to ignore content that consists purely of
-- whitespace. In fact, we don't expect both textual content and child nodes to
-- be appear inside a single element!
stripAllWhitespaceContent :: Document -> Document
stripAllWhitespaceContent Document {..} = Document {root = go root, ..}
  where
    go el = stripWhitespaceContent $ el {children = map goChild (children el)}
    goChild (NodeElement name el) = NodeElement name (go el)
    goChild (NodeContent content) = NodeContent content

-- | Most of the time you will want to use `stripAllWhitespaceContent` instead
-- to remove whitespace once at the top level! Use `stripWhitespaceContent` only
-- if you expect to mix content and elements in weird ways.
stripWhitespaceContent :: Element -> Element
stripWhitespaceContent el =
  el {children = filter (not . isWhitespaceContent) $ children el}
  where
    isWhitespaceContent (NodeContent text) = Text.all isSpace text
    isWhitespaceContent _ = False
