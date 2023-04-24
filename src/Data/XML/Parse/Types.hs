module Data.XML.Parse.Types
  ( -- * Parsers and errors
    Parser,
    ParserError (..),
    parserError,
    prettyParserError,
    ElementZipper (..),
    errorPath,
    documentWithZipper,
    elementWithZipper,

    -- * Documents
    FromDocument (..),
    fromRootElement,

    -- * Elements
    FromElement (..),
    OrEmpty (..),
    FromChoiceElement (..),

    -- * Content (in elements and attributes)
    FromContent (..),
    readContent,
    parseContentElement,
    parseContentElementPartially,
  )
where

import Control.Exception (Exception)
import Control.Monad.State.Lazy
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.XML.Types
import GHC.Stack
import Text.Read (readMaybe)
import Text.XML (Name)
import Type.Reflection (Typeable, typeRep)

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
    ) :
    errorPath ElementZipper {before = before {children = rest}, ..}
  [] -> case parent of
    Nothing -> []
    Just (parent', name) ->
      ("enter node \"" <> renderName name <> "\"") :
      errorPath parent'

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
  { callstack :: CallStack,
    info :: i,
    message :: String -- could turn into a sum?
  }
  deriving stock (Show)
  deriving anyclass (Exception)

parserError :: HasCallStack => i -> String -> ParserError i
parserError info message = ParserError {callstack = callStack, ..}

prettyParserError :: ParserError ElementZipper -> String
prettyParserError ParserError {..} =
  unlines
    ( message :
      "To get to the error location:" :
      map ("  " <>) (reverse $ errorPath info)
    )

type Parser i a = Either (ParserError i) a

class FromDocument a where
  fromDocument :: HasCallStack => AnnotatedDocument i -> Parser i a

fromRootElement :: (HasCallStack, FromElement b) => Name -> (b -> a) -> AnnotatedDocument i -> Parser i a
fromRootElement name f Document {..} =
  if rootName == name
    then f <$> fromElement root
    else
      Left . parserError info $
        "Failed to parse document: expected root element with name "
          <> renderName name
          <> " but got "
          <> renderName rootName

class FromElement a where
  fromElement :: HasCallStack => AnnotatedElement i -> Parser i a

-- | Textual 'content' appears either inside elements, e.g.
--
-- <element>Text</element>
--
-- or as the content of attributes:
--
-- <element attr_name="<attr_content>"/>
class FromContent a where
  fromContent :: HasCallStack => Text -> i -> Parser i a

readContent :: forall a i. (HasCallStack, Read a, Typeable a) => Text -> i -> Parser i a
readContent text i = case readMaybe $ Text.unpack text of
  Just a -> pure a
  Nothing -> do
    Left . parserError i $
      "Failed to read \""
        <> Text.unpack text
        <> "\" as "
        <> show (typeRep @a)

instance FromElement a => FromElement (OrEmpty a) where
  fromElement el = case fromElement el of
    Right a -> pure . OrEmpty $ Just a
    Left err ->
      if isEmptyElement el
        then pure $ OrEmpty Nothing
        else Left err

-- | A 'choice' element corresponds to the 'choice' schema element, which encodes sum types:
--
-- <example>
--  <A>... Something ...</A>
-- </example>
--
-- and
--
-- <example>
--  <B>... SomethingElse ...</B>
-- </example>
--
-- might both be instances of the sum type represented in Haskell as
--
-- data Example = A Something | B SomethingElse
class FromChoiceElement a where
  fromChoiceElement :: HasCallStack => Map Name (AnnotatedElement i -> Parser i a)

{- Instances -}
instance FromContent Text where
  fromContent t _ = pure t

instance FromContent Int where
  fromContent = readContent

instance FromElement Element where
  fromElement = pure . unAnnotateElement

instance FromDocument Document where
  fromDocument = pure . unAnnotateDocument

instance FromContent a => FromElement (ContentElement a) where
  fromElement el = ContentElement <$> parseContentElement fromContent el

deriving via ContentElement Text instance FromElement Text

deriving via ContentElement Int instance FromElement Int

-- | Expects an element consisting of a single content node or no child nodes
-- (which is treated as an empty content string). Fails if the element isn't
--  fully consumed, e.g. if the element has attributes.
parseContentElement ::
  HasCallStack => (Text -> i -> Parser i a) -> AnnotatedElement i -> Parser i a
parseContentElement p el@Element {info} = do
  (leftovers, a) <- parseContentElementPartially p el
  if isEmptyElement leftovers
    then pure a
    else Left $ parserError info "Failed to parse content element: element not completely consumed (unexpected attributes!)."

-- | Returns the remains (i.e. attributes).
parseContentElementPartially ::
  HasCallStack =>
  (Text -> i -> Parser i a) ->
  AnnotatedElement i ->
  Parser i (AnnotatedElement i, a)
parseContentElementPartially p el@Element {children, info} = do
  (content, i) <- case children of
    [NodeContent c i] -> pure (c, i)
    [] -> pure ("", info)
    _ -> Left $ parserError info "Failed to parse content element: unexpected child nodes."
  a <- p content i
  pure (el {children = []}, a)
