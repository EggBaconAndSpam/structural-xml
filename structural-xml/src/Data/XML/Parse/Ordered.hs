{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.XML.Parse.Ordered
  ( -- * A monad for parsing ordered elements
    OrderedM (..),
    parseOrderedElement,
    parseOrderedElementKeepLeftovers,
    parseOrderedElementPartially,

    -- * Combinators
    consumeAttribute,
    consumeOptionalAttribute,
    consumeElement,
    consumeElementOrAbsent,
    consumeElementOrEmpty,
    consumeElements,
    consumeRemainingElements,
    consumeLeftovers,

    -- * Re-exports
    module Data.XML.Types,
    module Data.XML.Parse.Types,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Data.Tuple
import Data.XML
import Data.XML.Parse.Types
import Data.XML.Types
import GHC.Stack
import Data.List.NonEmpty (NonEmpty)

{-
To be able to newtype derive FromChoiceElement we need the `a` parameter to be
`representational`, but with the following definition we get `nominal` instead.
-}
newtype OrderedM i a = OrderedM (StateT (AnnotatedElement i) (Either (NonEmpty (ParserError i))) a)
  deriving newtype (Functor, Applicative, Monad, MonadError (NonEmpty (ParserError i)), MonadState (AnnotatedElement i))

-- | Parse an 'ordered' element (corresponding to an xml 'sequence'). Fails if
-- the element is not fully consumed.
parseOrderedElement :: (HasCallStack) => OrderedM i a -> AnnotatedElement i -> Parser i a
parseOrderedElement p el@Element {info} = do
  (el', a) <- parseOrderedElementKeepLeftovers p el
  if isEmptyElement el'
    then pure a
    else
      throwError
        . parserError info
        $ "Element not fully consumed! Leftovers: " <> show (ReadShowXmlElement el')

-- | Parse an 'ordered' element (corresponding to an xml 'sequence'). Return the
-- rest of the element that wasn't consumed.
parseOrderedElementKeepLeftovers :: (HasCallStack) => OrderedM i a -> AnnotatedElement i -> Parser i (AnnotatedElement i, a)
parseOrderedElementKeepLeftovers (OrderedM p) el = swap <$> runStateT p el

-- | Parse an 'ordered' element (corresponding to an xml 'all'). Don't complain
-- about any unparsed nodes or attributes.
parseOrderedElementPartially :: (HasCallStack) => OrderedM i a -> AnnotatedElement i -> Parser i a
parseOrderedElementPartially p el = snd <$> parseOrderedElementKeepLeftovers p el

consumeOptionalAttribute :: (HasCallStack, FromContent a) => Name -> OrderedM i (Maybe a)
consumeOptionalAttribute name = do
  el@Element {attributes} <- get
  case Map.lookup name attributes of
    Nothing -> pure Nothing
    Just (text, i) -> do
      a <- either throwError pure $ fromContent text i
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
          a <- either throwError pure $ fromElement el
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
          a <- either throwError pure $ fromElement el
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
    [] -> throwError . parserError info $ "Missing node (reached end of element): " <> renderName name

-- | Parses `Maybe a` via `MaybeEmpty a`.
consumeElementOrEmpty :: (HasCallStack, FromElement a) => Name -> OrderedM i (Maybe a)
consumeElementOrEmpty name = unMaybeEmpty <$> consumeElement name

consumeElements :: (HasCallStack, FromElement a) => Name -> OrderedM i [a]
consumeElements name = do
  ma <- consumeElementOrAbsent name
  case ma of
    Nothing -> pure []
    Just a -> (a :) <$> consumeElements name

{-
consumeChoiceElement :: forall a i. (HasCallStack, FromChoiceElement a) => OrderedM i a
consumeChoiceElement = do
  remaining@Element {children, info} <- get
  case children of
    NodeElement name el i : rest
      | Just p <- Map.lookup name fromChoiceElement -> do
        a <- either throwError pure $ p el
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
-}
consumeRemainingElements :: (HasCallStack) => OrderedM i [(Name, AnnotatedElement i)]
consumeRemainingElements = do
  remaining@Element {children} <- get
  result <- forM children $ \case
    NodeElement name el _ -> pure (name, el)
    NodeContent _ i ->
      throwError . parserError i $
        "Unexpected content node when parsing ordered element! When parsing remaining child elements."
  put $ remaining {children = []}
  pure result

consumeLeftovers :: OrderedM i Leftovers
consumeLeftovers = do
  remaining <- get
  put remaining {children = mempty, attributes = mempty}
  pure . Leftovers $ unAnnotateElement remaining
