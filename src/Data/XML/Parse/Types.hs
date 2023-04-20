module Data.XML.Parse.Types where

import Control.Exception (Exception)
import Control.Monad.State.Lazy
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.XML.Types (renderName)
import qualified Data.XML.Types as XML
import GHC.Stack
import Text.XML (Name)

data Document i = Document
  { rootName :: Name,
    root :: Element i,
    info :: i
  }
  deriving stock (Show, Eq, Ord)

isEmptyElement :: Element i -> Bool
isEmptyElement Element {..} = null attributes && null children

data Element i = Element
  { attributes :: Map Name (Text, i),
    children :: [Node i],
    info :: ~i
  }
  deriving stock (Show, Eq, Ord)

data Node i
  = NodeElement Name (Element i) i
  | NodeContent (Text, i)
  deriving stock (Show, Eq, Ord)

documentWithoutInfo :: Document i -> XML.Document
documentWithoutInfo Document {..} =
  XML.Document
    { root = elementWithoutInfo root,
      ..
    }

elementWithoutInfo :: Element i -> XML.Element
elementWithoutInfo Element {..} =
  XML.Element
    { attributes = Map.map fst attributes,
      children = map nodeWithoutInfo children
    }
  where
    nodeWithoutInfo = \case
      NodeContent (text, _) -> XML.NodeContent text
      NodeElement name el _ -> XML.NodeElement name (elementWithoutInfo el)

documentWithTrivialInfo :: XML.Document -> Document ()
documentWithTrivialInfo XML.Document {..} =
  Document
    { root = elementWithTrivialInfo root,
      info = (),
      ..
    }

elementWithTrivialInfo :: XML.Element -> Element ()
elementWithTrivialInfo XML.Element {..} =
  Element
    { attributes = Map.map (\t -> (t, ())) attributes,
      children = map nodeWithTrivialInfo children,
      info = ()
    }
  where
    nodeWithTrivialInfo = \case
      XML.NodeContent text -> NodeContent (text, ())
      XML.NodeElement name el -> NodeElement name (elementWithTrivialInfo el) ()

-- Almost as good as textual locations...
data ElementZipper = ElementZipper
  { parent :: Maybe (ElementZipper, Name),
    before :: XML.Element, -- reversed children
    after :: XML.Element
  }

-- move this somewhere else?
zipperPath :: ElementZipper -> [Text]
zipperPath ElementZipper {parent, ..} = case XML.children before of
  child : rest ->
    ( case child of
        XML.NodeElement name _ -> "skip node \"" <> renderName name <> "\""
        XML.NodeContent _ -> "skip content node"
    )
      : zipperPath ElementZipper {before = before {XML.children = rest}, ..}
  [] -> case parent of
    Nothing -> []
    Just (parent', name) ->
      ("enter node \"" <> renderName name <> "\"")
        : zipperPath parent'

-- For slightly more useful errors...
documentWithZipper :: XML.Document -> Document ElementZipper
documentWithZipper XML.Document {..} =
  Document
    { root = elementWithZipper (Just (rootZipper, rootName)) root,
      info = rootZipper,
      ..
    }
  where
    rootZipper = ElementZipper {parent = Nothing, before = XML.emptyElement, after = root}

elementWithZipper :: Maybe (ElementZipper, Name) -> XML.Element -> Element ElementZipper
elementWithZipper parent el = do
  Element
    { attributes = Map.map (\t -> (t, initialState)) $ XML.attributes el,
      children = evalState (mapM nodeWithZipper $ XML.children el) initialState,
      info = initialState
    }
  where
    initialState = ElementZipper {before = XML.emptyElement, after = el, parent}

    nodeWithZipper node = do
      current@ElementZipper {before, after} <- get
      put $
        current
          { before = XML.Element {attributes = mempty, children = [node]} <> before,
            after = after {XML.children = drop 1 $ XML.children after}
          }
      case node of
        XML.NodeContent text -> pure $ NodeContent (text, current)
        XML.NodeElement name child -> pure $ NodeElement name (elementWithZipper (Just (current, name)) child) current

data ParserError i = ParserError
  { callstack :: CallStack, -- todo
    info :: i,
    message :: Text -- could turn into a sum?
  }
  deriving stock (Show)
  deriving anyclass (Exception)

parserError :: HasCallStack => i -> Text -> ParserError i
parserError info message = ParserError {callstack = callStack, ..}

-- | todo: Tries to parse the element. If the parser fails and the element is empty
-- then returns Nothing. better: modify parser?
newtype OrEmpty a = OrEmpty {unOrEmpty :: Maybe a}
