{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.XML.Parse.Ordered
  ( -- * A monad for parsing ordered elements
    OrderedM (..),
    parseOrderedElement,
    parseOrderedElementLax,

    -- * Combinators
    consumeAttribute,
    consumeOptionalAttribute,
    consumeElement,
    consumeElementOrAbsent,
    consumeElementOrEmpty,
    consumeElements,
    consumeChoiceElement,
    consumeRemainingElements,

    -- * Re-exports
    module Data.XML.Types,
    module Data.XML.Parse.Types,
  )
where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Tuple
import Data.XML.Parse.Types
import Data.XML.Types
import GHC.Stack
import Text.XML (Name)

-- | todo: state for keeping track of remaining nodes and attributes.
newtype OrderedM i a = OrderedM (StateT (AnnotatedElement i) (Either (ParserError i)) a)
  deriving newtype (Functor, Applicative, Monad, MonadError (ParserError i), MonadState (AnnotatedElement i))

-- | Parse an 'ordered' element (corresponding to an xml 'sequence'). Fails if
-- the element is not fully consumed.
parseOrderedElement :: HasCallStack => OrderedM i a -> AnnotatedElement i -> Parser i a
parseOrderedElement p el@Element {info} = do
  (el', a) <- parseOrderedElementLax p el
  if isEmptyElement el'
    then pure a
    else throwError $ parserError info "Element not fully consumed!" -- todo: include unparsed remainder!

-- | Parse an 'ordered' element (corresponding to an xml 'sequence'). Return the
-- rest of the element that wasn't consumed.
parseOrderedElementLax :: HasCallStack => OrderedM i a -> AnnotatedElement i -> Parser i (AnnotatedElement i, a)
parseOrderedElementLax (OrderedM p) el = swap <$> runStateT p el

consumeOptionalAttribute :: (HasCallStack, FromContent a) => Name -> OrderedM i (Maybe a)
consumeOptionalAttribute name = do
  el@Element {attributes} <- get
  case Map.lookup name attributes of
    Nothing -> pure Nothing
    Just (text, i) -> do
      a <- OrderedM . lift $ fromContent text i
      put $ el {attributes = Map.delete name attributes}
      pure $ Just a

consumeAttribute :: (HasCallStack, FromContent a) => Name -> OrderedM i a
consumeAttribute name = do
  ma <- consumeOptionalAttribute name
  case ma of
    Just a -> pure a
    Nothing -> do
      Element {info} <- get
      throwError . parserError info $ "Missing attribute: " <> renderName name

consumeElementOrAbsent :: (HasCallStack, FromElement a) => Name -> OrderedM i (Maybe a)
consumeElementOrAbsent name = do
  remaining@Element {children} <- get
  case children of
    NodeElement name' el _ : rest
      | name == name' -> do
        a <- OrderedM . lift $ fromElement el
        put $ remaining {children = rest}
        pure $ Just a
      | otherwise -> pure Nothing
    NodeContent _ i : _ ->
      throwError . parserError i $
        "Unexpected content node when parsing ordered element! When parsing (potentially absent) node: "
          <> renderName name
    _ -> pure Nothing

consumeElement :: (HasCallStack, FromElement a) => Name -> OrderedM i a
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
            <> renderName name
            <> " but found "
            <> renderName name'
            <> " instead"
    NodeContent _ i : _ ->
      throwError . parserError i $
        "Unexpected content node when parsing ordered element! Expected node: "
          <> renderName name
    [] -> throwError $ parserError info $ "Missing node (reached end of element): " <> renderName name

-- | Parses `Maybe a` via `OrEmpty a`.
consumeElementOrEmpty :: (HasCallStack, FromElement a) => Name -> OrderedM i (Maybe a)
consumeElementOrEmpty name = unOrEmpty <$> consumeElement name

consumeElements :: (HasCallStack, FromElement a) => Name -> OrderedM i [a]
consumeElements name = do
  ma <- consumeElementOrAbsent name
  case ma of
    Nothing -> pure []
    Just a -> (a :) <$> consumeElements name

consumeChoiceElement :: forall a i. (HasCallStack, FromChoiceElement a) => OrderedM i a
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
            <> (intercalate ", " . map renderName . Map.keys $ fromChoiceElement @a)
            <> " but found "
            <> renderName name
            <> " instead"
    NodeContent _ i : _ ->
      throwError . parserError i $
        "Unexpected content node when parsing ordered element! Expected (one of) "
          <> (intercalate ", " . map renderName . Map.keys $ fromChoiceElement @a)
    [] ->
      throwError . parserError info $
        "Missing choice node (one of): "
          <> (intercalate ", " . map renderName . Map.keys $ fromChoiceElement @a)

consumeRemainingElements :: HasCallStack => OrderedM i [(Name, AnnotatedElement i)]
consumeRemainingElements = do
  remaining@Element {children} <- get
  result <- forM children $ \case
    NodeElement name el _ -> pure (name, el)
    NodeContent _ i ->
      throwError . parserError i $
        "Unexpected content node when parsing ordered element! When parsing remaining child elements."
  put $ remaining {children = []}
  pure result
