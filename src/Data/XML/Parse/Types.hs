module Data.XML.Parse.Types
  ( Parser,
    ParserError (..),
    parserError,
    prettyParserError,
    prettyParserErrorWithCallStack,
    FromDocument (..),
    fromRootElement,
    FromElement (..),
    FromChoiceElement (..),
    FromContent (..),
    readContent,
    parseContentElement,
    parseContentElementPartially,
    parseContentElementKeepLeftovers,
  )
where

import Control.Exception (Exception)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.XML.Parse.Location
import Data.XML.Types
import GHC.Stack
import Text.Read (readMaybe)
import Text.XML (Name)
import Type.Reflection (Typeable, typeRep)

data ParserError i = ParserError
  { callstack :: CallStack,
    info :: i,
    message :: String -- could turn into a sum?
  }
  deriving stock (Show)
  deriving anyclass (Exception)

parserError :: HasCallStack => i -> String -> ParserError i
parserError info message = ParserError {callstack = callStack, ..}

prettyParserError :: ParserError Location -> String
prettyParserError ParserError {..} =
  unlines
    ( message :
      "To get to the error location:" :
      map ("  " <>) (printPath info)
    )

prettyParserErrorWithCallStack :: ParserError Location -> String
prettyParserErrorWithCallStack err@ParserError {callstack} =
  prettyParserError err <> prettyCallStack callstack

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
  (leftovers, a) <- parseContentElementKeepLeftovers p el
  if isEmptyElement leftovers
    then pure a
    else Left $ parserError info "Failed to parse content element: element not completely consumed (unexpected attributes!)."

-- | Returns the leftovers (i.e. any attributes).
parseContentElementKeepLeftovers ::
  HasCallStack =>
  (Text -> i -> Parser i a) ->
  AnnotatedElement i ->
  Parser i (AnnotatedElement i, a)
parseContentElementKeepLeftovers p el@Element {children, info} = do
  (content, i) <- case children of
    [NodeContent c i] -> pure (c, i)
    [] -> pure ("", info)
    _ -> Left $ parserError info "Failed to parse content element: unexpected child nodes."
  a <- p content i
  pure (el {children = []}, a)

-- | Ignores attributes.
parseContentElementPartially ::
  HasCallStack =>
  (Text -> i -> Parser i a) ->
  AnnotatedElement i ->
  Parser i a
parseContentElementPartially p el =
  snd <$> parseContentElementKeepLeftovers p el
