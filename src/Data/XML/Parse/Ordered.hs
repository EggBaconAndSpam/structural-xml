module Data.XML.Parse.Ordered
  ( parseOrderedElement,
    parseOrderedElementLax,
    consumeAttribute,
    consumeOptionalAttribute,
    consumeElement,
    consumeElementOrAbsent,
    consumeElementOrEmpty,
    consumeElements,
    consumeChoiceElement,
    stripAllWhitespaceContent,
    stripWhitespaceContent,
  )
where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Tuple
import Data.XML.Parse.Class
import Data.XML.Parse.Types
import Text.XML (Name)

newtype OrderedM i a = OrderedM (StateT (Element i) (Either (ParserError i)) a)
  deriving newtype (Functor, Applicative, Monad, MonadError (ParserError i), MonadState (Element i))

-- | Most of the time we want to ignore content that consists purely of
-- whitespace. In fact, we don't expect both textual content and child nodes to
-- be appear inside a single element usually!
stripAllWhitespaceContent :: Document i -> Document i
stripAllWhitespaceContent Document {..} = Document {root = go root, ..}
  where
    go el = stripWhitespaceContent $ el {children = map goChild (children el)}
    goChild (NodeElement name el i) = NodeElement name (go el) i
    goChild (NodeContent content) = NodeContent content

-- | Most of the time you will want to use `stripAllWhitespaceContent` instead
-- to remove whitespace once at the top level! Use `stripWhitespaceContent` only
-- if you expect to mix content and elements in weird ways.
stripWhitespaceContent :: Element i -> Element i
stripWhitespaceContent el =
  el {children = filter (not . isWhitespaceContent) $ children el}
  where
    isWhitespaceContent (NodeContent (text, _)) = Text.all isSpace text
    isWhitespaceContent _ = False

-- | Parse an 'ordered' element (corresponding to an xml 'sequence'). Fails if
-- the element is not fully consumed.
parseOrderedElement :: OrderedM i a -> Element i -> Parser i a
parseOrderedElement p el@Element {info} = do
  (el', a) <- parseOrderedElementLax p el
  if isEmptyElement el'
    then pure a
    else throwError $ parserError info "Element not fully consumed!" -- todo: include unparsed remainder!

-- | Parse an 'ordered' element (corresponding to an xml 'sequence'). Return the
-- rest of the element that wasn't consumed.
parseOrderedElementLax :: OrderedM i a -> Element i -> Parser i (Element i, a)
parseOrderedElementLax (OrderedM p) el = swap <$> runStateT p el

consumeOptionalAttribute :: FromContent a => Name -> OrderedM i (Maybe a)
consumeOptionalAttribute name = do
  el@Element {attributes} <- get
  case Map.lookup name attributes of
    Nothing -> pure Nothing
    Just (text, i) -> do
      a <- OrderedM . lift $ fromContent (text, i)
      put $ el {attributes = Map.delete name attributes}
      pure $ Just a

consumeAttribute :: FromContent a => Name -> OrderedM i a
consumeAttribute name = do
  ma <- consumeOptionalAttribute name
  case ma of
    Just a -> pure a
    Nothing -> do
      Element {info} <- get
      throwError . parserError info $ "Missing attribute: " <> Text.pack (show name)

consumeElementOrAbsent :: FromElement a => Name -> OrderedM i (Maybe a)
consumeElementOrAbsent name = do
  remaining@Element {children} <- get
  case children of
    NodeElement name' el _ : rest
      | name == name' -> do
          a <- OrderedM . lift $ fromElement el
          put $ remaining {children = rest}
          pure $ Just a
      | otherwise -> pure Nothing
    NodeContent (_, i) : _ ->
      throwError . parserError i $
        "Unexpected content node when parsing ordered element! When parsing (potentially absent) node: "
          <> Text.pack (show name)
    _ -> pure Nothing

consumeElement :: FromElement a => Name -> OrderedM i a
consumeElement name = do
  remaining@Element {children, info} <- get
  case children of
    NodeElement name' el i : rest
      | name == name' -> do
          a <- OrderedM . lift $ fromElement el
          put $ remaining {children = rest}
          pure a
      | otherwise ->
          throwError . parserError i $
            "Unexpected node! Expected "
              <> Text.pack (show name)
              <> " but found "
              <> Text.pack (show name)
              <> " instead"
    NodeContent (_, i) : _ ->
      throwError . parserError i $
        "Unexpected content node when parsing ordered element! Expected node: "
          <> Text.pack (show name)
    [] -> throwError $ parserError info $ "Missing node (reached end of element): " <> Text.pack (show name)

-- | todo: parses and unwraps `OrEmpty a`
consumeElementOrEmpty :: FromElement a => Name -> OrderedM i (Maybe a)
consumeElementOrEmpty name = unOrEmpty <$> consumeElement name

consumeElements :: FromElement a => Name -> OrderedM i [a]
consumeElements name = do
  ma <- consumeElementOrAbsent name
  case ma of
    Nothing -> pure []
    Just a -> (a :) <$> consumeElements name

consumeChoiceElement :: forall a i. FromChoiceElement a => OrderedM i a
consumeChoiceElement = do
  remaining@Element {children, info} <- get
  case children of
    NodeElement name el i : rest
      | Just p <- Map.lookup name fromChoiceElement -> do
          a <- OrderedM . lift $ p el
          put $ remaining {children = rest}
          pure a
      | otherwise ->
          throwError . parserError i $
            "Unexpected node! Expected (one of) "
              <> Text.pack (show . Map.keys $ fromChoiceElement @a)
              <> " but found "
              <> Text.pack (show name)
              <> " instead"
    NodeContent (_, i) : _ ->
      throwError . parserError i $
        "Unexpected content node when parsing ordered element! Expected (one of) "
          <> Text.pack (show . Map.keys $ fromChoiceElement @a)
    [] ->
      throwError . parserError info $
        "Missing choice node (one of): "
          <> Text.pack (show . Map.keys $ fromChoiceElement @a)
