module Data.XML.Deserialisation
  ( FromDocument (..),
    FromElement (..),
    FromContent (..),
    FromChoiceElement (..),
    ParserError (..),
    AttributeConsumer (..),
    ElementConsumer (..),
    parseOrderedElement,
    parseUnorderedElement,
    parseContentElement,
    readContent,
    throwParserError,
    fromElement',
  )
where

import Control.Exception (Exception)
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Char (isSpace)
import Data.List (delete)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.XML.Helpers.ContentElement
import Data.XML.Types
import GHC.Stack
import Text.Read (readMaybe)
import Text.XML (Name)
import Type.Reflection (Typeable, typeRep)

{- Classes -}

class FromDocument a where
  fromDocument :: Document -> Either ParserError a

class FromElement a where
  fromElement :: HasCallStack => Element -> Either ParserError a

fromElement' :: FromElement a => Element -> Maybe a
fromElement' = either (const Nothing) Just . fromElement

-- | Textual 'content' appears either inside elements, e.g.
--
-- <element>Text</element>
--
-- or as the content of attributes:
--
-- <element attr_name="<attr_content>"/>
class FromContent a where
  fromContent :: HasCallStack => Text -> Either ParserError a

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
  fromElementChoice :: [(Name, Element -> Either ParserError a)]

{- Instances -}

instance FromContent Text where
  fromContent = pure

instance FromContent Int where
  fromContent t = case readMaybe $ Text.unpack t of
    Nothing -> throwParserError $ "Failed to read " <> t <> " as Int"
    Just i -> pure i

instance FromElement Element where
  fromElement = pure

instance FromContent a => FromElement (ContentElement a) where
  fromElement el = ContentElement <$> parseContentElement (liftEither . fromContent) el

deriving via ContentElement Text instance FromElement Text

deriving via ContentElement Int instance FromElement Int

{- Parsing elements -}

data ParserError = ParserError
  { callstack :: CallStack,
    message :: Text
  }
  deriving stock (Show)
  deriving anyclass (Exception)

class MonadError ParserError m => AttributeConsumer m where
  consumeOptionalAttribute :: FromContent a => Name -> m (Maybe a)
  consumeRemainingAttributes :: m (Map Name Text)

  consumeAttribute :: FromContent a => Name -> m a
  consumeAttribute name =
    consumeOptionalAttribute name >>= \case
      Nothing -> throwParserError $ "Missing " <> Text.pack (show name) <> "attribute"
      Just a -> pure a

class MonadError ParserError m => ElementConsumer m where
  consumeElementOrAbsent :: FromElement a => Name -> m (Maybe a)
  consumeRemainingElements :: m [(Name, Element)]

  consumeElement :: FromElement a => Name -> m a
  consumeElement name = do
    consumeElementOrAbsent name >>= \case
      Nothing -> throwParserError $ "Missing " <> Text.pack (show name) <> " element."
      Just a -> pure a

  consumeElementOrEmpty :: FromElement a => Name -> m (Maybe a)
  consumeElementOrEmpty name = do
    el <- consumeElement name
    if el == emptyElement
      then pure Nothing
      else Just <$> liftEither (fromElement el)

  consumeElements :: FromElement a => Name -> m [a]
  consumeElements name = do
    consumeElementOrAbsent name >>= \case
      Nothing -> pure []
      Just a -> (a :) <$> consumeElements name

  consumeChoiceElement :: FromChoiceElement a => m a
  consumeChoiceElement = go fromElementChoice
    where
      go [] = throwParserError "No constructor of sum type matched."
      go ((name, parser) : ps) =
        consumeElementOrAbsent name >>= \case
          Nothing -> go ps
          Just el -> liftEither (parser el)

-- | Parse an 'ordered' element (corresponding to an xml 'sequence'). Strips
-- whitespace content. Fails if the element is not fully consumed.
parseOrderedElement ::
  (forall m. (AttributeConsumer m, ElementConsumer m, MonadError ParserError m) => m a) ->
  Element ->
  Either ParserError a
parseOrderedElement (OrderedElementParser p) el = case runStateT p (stripWhitespaceContent el) of
  Left err -> Left err
  Right (a, el') ->
    if el' == emptyElement
      then Right a
      else throwParserError "element not fully consumed" -- todo: better error messages? include unparsed remainder

-- | Parse an 'unordered' element (corresponding to xml 'all'). Strips
-- whitespace content. Fails if the element is not fully consumed.
parseUnorderedElement ::
  (forall m. (AttributeConsumer m, ElementConsumer m, MonadError ParserError m) => m a) ->
  Element ->
  Either ParserError a
parseUnorderedElement (UnorderedElementParser p) el = case runStateT p (stripWhitespaceContent el) of
  Left err -> Left err
  Right (a, el') ->
    if el' == emptyElement
      then Right a
      else throwParserError "element not fully consumed" -- todo: better error messages? include unparsed remainder

-- | Expects an element consisting of a single content node. Fails if attributes
-- aren't fully consumed.
parseContentElement ::
  (forall m. (AttributeConsumer m, MonadError ParserError m) => Text -> m a) ->
  Element ->
  Either ParserError a
parseContentElement p el = do
  content <- case children el of
    [NodeContent c] -> pure c
    [] -> throwParserError "Empty content element."
    _ -> throwParserError "Content element must contain exactly one content node and no child elements."
  let ContentParser p' = p content
  case runStateT p' (el {children = []}) of
    Left err -> Left err
    Right (a, el') ->
      if el' == emptyElement
        then Right a
        else throwParserError "element not fully consumed"

newtype OrderedElementParser a = OrderedElementParser (StateT Element (Either ParserError) a)
  deriving newtype (Functor, Applicative, Monad, MonadError ParserError)

instance AttributeConsumer OrderedElementParser where
  consumeOptionalAttribute name = OrderedElementParser $ do
    atts <- gets attributes
    ma <- case Map.lookup name atts of
      Nothing -> pure Nothing
      Just text -> Just <$> liftEither (fromContent text)
    modify (\el -> el {attributes = Map.delete name (attributes el)})
    pure ma

  consumeRemainingAttributes = OrderedElementParser $ do
    atts <- gets attributes
    modify (\el -> el {attributes = Map.empty})
    pure atts

instance ElementConsumer OrderedElementParser where
  consumeElementOrAbsent name = OrderedElementParser $ do
    children <- gets children
    case children of
      [] -> pure Nothing
      NodeContent _ : _ -> throwParserError "An 'ordered' element can't contain content nodes."
      NodeElement name' el : rest -> do
        if name /= name'
          then pure Nothing
          else do
            a <- liftEither $ fromElement el
            modify (\el' -> el' {children = rest})
            pure $ Just a

  consumeRemainingElements = OrderedElementParser $ do
    children <- gets children
    modify (\el -> el {children = []})
    forM children $ \case
      NodeContent _ -> throwParserError "An 'ordered' element can't contain content nodes."
      NodeElement name el -> pure (name, el)

newtype UnorderedElementParser a = UnorderedElementParser (StateT Element (Either ParserError) a)
  deriving newtype (Functor, Applicative, Monad, MonadError ParserError)
  deriving (AttributeConsumer) via OrderedElementParser

instance ElementConsumer UnorderedElementParser where
  consumeElementOrAbsent name = UnorderedElementParser $ do
    children <- gets children
    let matchElementName (NodeElement name' el) = if name == name' then Just el else Nothing
        matchElementName (NodeContent _) = Nothing
    case mapMaybe matchElementName children of
      [] -> pure Nothing
      (el : _) -> do
        a <- liftEither $ fromElement el
        modify (\el' -> el' {children = delete (NodeElement name el) children})
        pure $ Just a

  consumeRemainingElements = UnorderedElementParser $ do
    children <- gets children
    modify (\el -> el {children = []})
    forM children $ \case
      NodeContent _ -> throwParserError "An 'ordered' element can't contain content nodes."
      NodeElement name el -> pure (name, el)

newtype ContentParser a = ContentParser (StateT Element (Either ParserError) a)
  deriving newtype (Functor, Applicative, Monad, MonadError ParserError)
  deriving (AttributeConsumer) via OrderedElementParser

newtype ElementParser a = ElementParser (StateT Element (Either ParserError) a)
  deriving newtype (Functor, Applicative, Monad, MonadError ParserError)

throwParserError :: (MonadError ParserError m, HasCallStack) => Text -> m a
throwParserError t = throwError $ ParserError {callstack = callStack, message = t}

-- Whitespace only has semantic meaning in "content" nodes.
stripWhitespaceContent :: Element -> Element
stripWhitespaceContent el =
  el {children = filter (not . isWhitespaceContent) $ children el}
  where
    isWhitespaceContent (NodeContent text) = Text.all isSpace text
    isWhitespaceContent _ = False

readContent :: forall a m. (Typeable a, Read a, MonadError ParserError m) => Text -> m a
readContent text = do
  case readMaybe $ Text.unpack text of
    Just a -> pure a
    Nothing -> do
      let typename = Text.pack . show $ typeRep @a
      throwParserError $ "Failed to read " <> text <> " as " <> typename
