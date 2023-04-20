module Data.XML.Parse.Types where

import Control.Exception (Exception)
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map
import Data.XML.Types
import GHC.Stack
import Text.XML (Name)

-- | Almost as good as textual locations...
data ElementZipper = ElementZipper
  { parent :: Maybe (ElementZipper, Name),
    before :: Element, -- reversed children
    after :: Element
  }

errorPath :: ElementZipper -> [String]
errorPath ElementZipper {parent, ..} = case children before of
  child : rest ->
    ( case child of
        NodeElement name _ _ -> "skip node \"" <> renderName name <> "\""
        NodeContent _ _ -> "skip content node"
    )
      : errorPath ElementZipper {before = before {children = rest}, ..}
  [] -> case parent of
    Nothing -> []
    Just (parent', name) ->
      ("enter node \"" <> renderName name <> "\"")
        : errorPath parent'

documentWithZipper :: Document -> AnnotatedDocument ElementZipper
documentWithZipper Document {..} =
  Document
    { root = elementWithZipper (Just (rootZipper, rootName)) root,
      info = rootZipper,
      ..
    }
  where
    rootZipper = ElementZipper {parent = Nothing, before = emptyElement, after = root}

elementWithZipper :: Maybe (ElementZipper, Name) -> Element -> AnnotatedElement ElementZipper
elementWithZipper parent el = do
  Element
    { attributes = Map.map (\(t, _) -> (t, initialState)) $ attributes el,
      children = evalState (mapM nodeWithZipper $ children el) initialState,
      info = initialState
    }
  where
    initialState = ElementZipper {before = emptyElement, after = el, parent}

    nodeWithZipper node = do
      current@ElementZipper {before, after} <- get
      put $
        current
          { before = Element {attributes = mempty, children = [node], info = ()} <> before,
            after = after {children = drop 1 $ children after}
          }
      case node of
        NodeContent text _ -> pure $ NodeContent text current
        NodeElement name child _ -> pure $ NodeElement name (elementWithZipper (Just (current, name)) child) current

data ParserError i = ParserError
  { callstack :: CallStack, -- todo
    info :: i,
    message :: String -- could turn into a sum?
  }
  deriving stock (Show)
  deriving anyclass (Exception)

parserError :: HasCallStack => i -> String -> ParserError i
parserError info message = ParserError {callstack = callStack, ..}
