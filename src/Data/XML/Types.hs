module Data.XML.Types where

import Data.Char (isSpace)
import qualified Data.Map as Map
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

data AnnotatedDocument i = Document
  { rootName :: Name,
    root :: AnnotatedElement i,
    info :: i
  }
  deriving stock (Show, Eq, Ord, Generic)

-- We move element names one level up to the parent element.
data AnnotatedElement i = Element
  { attributes :: Map Name (Text, i),
    children :: [AnnotatedNode i],
    info :: i
  }
  deriving stock (Show, Eq, Ord, Generic)

type Document = AnnotatedDocument ()

type Element = AnnotatedElement ()

type Node = AnnotatedNode ()

data AnnotatedNode i
  = NodeElement Name (AnnotatedElement i) i
  | NodeContent Text i
  deriving stock (Show, Eq, Ord, Generic)

-- | Helper for elements that only contain content. Has To/FromElement instances
-- defined in terms of To/FromContent.
newtype ContentElement a = ContentElement {content :: a}
  deriving stock (Show, Eq, Ord, Generic)

-- | Encodes `Nothing` as emptyElement. Parses the empty element as `Nothing`,
-- unless the parser for `a` succeeds on the empty element.
newtype OrEmpty a = OrEmpty {unOrEmpty :: Maybe a}

instance Semigroup Element where
  el <> el' =
    Element
      { attributes = attributes el <> attributes el',
        children = children el <> children el',
        info = ()
      }

instance Monoid Element where
  mempty = emptyElement

emptyElement :: Element
emptyElement = Element mempty [] ()

isEmptyElement :: AnnotatedElement i -> Bool
isEmptyElement Element {..} = null attributes && null children

unAnnotateDocument :: AnnotatedDocument i -> Document
unAnnotateDocument Document {..} =
  Document {root = unAnnotateElement root, rootName, info = ()}

unAnnotateElement :: AnnotatedElement i -> Element
unAnnotateElement Element {..} =
  Element
    { attributes = Map.map (\(t, _) -> (t, ())) attributes,
      children = map nodeWithoutInfo children,
      info = ()
    }
  where
    nodeWithoutInfo = \case
      NodeContent text _ -> NodeContent text ()
      NodeElement name el _ -> NodeElement name (unAnnotateElement el) ()

{- Conversion from and to xml-conduit -}

-- | Strips whitespace content since we don't usually care about that. If you
-- want a lossless conversion instead use fromXmlConduitKeepWhitespaceElements.
fromXmlConduit :: XC.Document -> Document
fromXmlConduit = stripAllWhitespaceContent . fromXmlConduitKeepWhitespaceElements

fromXmlConduitKeepWhitespaceElements :: XC.Document -> Document
fromXmlConduitKeepWhitespaceElements doc =
  Document
    { rootName = XC.elementName $ XC.documentRoot doc,
      root = fromXmlConduitElement $ XC.documentRoot doc,
      info = ()
    }

fromXmlConduitElement :: XC.Element -> Element
fromXmlConduitElement el =
  Element
    { attributes = Map.map (\t -> (t, ())) $ XC.elementAttributes el,
      children = mapMaybe fromXmlConduitNode $ XC.elementNodes el,
      info = ()
    }
  where
    fromXmlConduitNode :: XC.Node -> Maybe Node
    fromXmlConduitNode (XC.NodeContent t) = Just $ NodeContent t ()
    fromXmlConduitNode (XC.NodeElement e) =
      Just $ NodeElement (XC.elementName e) (fromXmlConduitElement e) ()
    fromXmlConduitNode _ = Nothing

toXmlConduit :: AnnotatedDocument i -> XC.Document
toXmlConduit Document {..} =
  XC.Document
    { documentPrologue = XC.Prologue [] Nothing [],
      documentRoot = toXmlConduitElement rootName root,
      documentEpilogue = []
    }

toXmlConduitElement :: Name -> AnnotatedElement i -> XC.Element
toXmlConduitElement name Element {..} =
  XC.Element
    { elementName = name,
      elementAttributes = Map.map fst attributes,
      elementNodes = map toXmlConduitNode children
    }
  where
    toXmlConduitNode :: AnnotatedNode i -> XC.Node
    toXmlConduitNode (NodeContent t _) = XC.NodeContent t
    toXmlConduitNode (NodeElement n el _) = XC.NodeElement $ toXmlConduitElement n el

-- | Most of the time we want to ignore content that consists purely of
-- whitespace. In fact, we don't expect both textual content and child nodes to
-- be appear inside a single element!
stripAllWhitespaceContent :: AnnotatedDocument i -> AnnotatedDocument i
stripAllWhitespaceContent Document {..} = Document {root = go root, ..}
  where
    go el = stripWhitespaceContent $ el {children = map goChild (children el)}
    goChild (NodeElement name el i) = NodeElement name (go el) i
    goChild (NodeContent content i) = NodeContent content i

-- | Most of the time you will want to use `stripAllWhitespaceContent` instead
-- to remove whitespace once at the top level! Use `stripWhitespaceContent` only
-- if you expect to mix content and elements in weird ways.
stripWhitespaceContent :: AnnotatedElement i -> AnnotatedElement i
stripWhitespaceContent el =
  el {children = filter (not . isWhitespaceContent) $ children el}
  where
    isWhitespaceContent (NodeContent text _) = Text.all isSpace text
    isWhitespaceContent _ = False
