{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.XML.Parse.Unordered
  ( -- * A monad for parsing unordered elements
    UnorderedM (..),
    parseUnorderedElement,
    parseUnorderedElementPartially,
    parseUnorderedElementKeepLeftovers,

    -- * Combinators
    consumeAttribute,
    consumeOptionalAttribute,
    consumeElement,
    consumeElementOrAbsent,
    consumeElementOrEmpty,
    consumeElements,
    -- consumeChoiceElement,
    consumeRemainingElements,
    consumeLeftovers,

    -- * Re-exports
    module Data.XML.Types,
    module Data.XML.Parse.Types,
  )
where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Tuple
import Data.XML
import Data.XML.Parse.Types
import Data.XML.Types
import GHC.Stack

-- Quadratic behaviour in number of children. Could be improved, but probably not an issue.
newtype UnorderedM i a = UnorderedM (StateT (AnnotatedElement i) (Either (NonEmpty (ParserError i))) a)
  deriving newtype (Functor, Applicative, Monad, MonadError (NonEmpty (ParserError i)), MonadState (AnnotatedElement i))

-- | Parse an 'ordered' element (corresponding to an xml 'sequence'). Fails if
-- the element is not fully consumed.
parseUnorderedElement :: (HasCallStack) => UnorderedM i a -> AnnotatedElement i -> Parser i a
parseUnorderedElement p el@Element {info} = do
  (el', a) <- parseUnorderedElementKeepLeftovers p el
  if isEmptyElement el'
    then pure a
    else
      throwError
        . parserError info
        $ "Element not fully consumed! Leftovers: " <> show (ReadShowXmlElement el')

-- | Parse an 'unordered' element (corresponding to an xml 'all'). Return the
-- rest of the element that wasn't consumed.
parseUnorderedElementKeepLeftovers :: (HasCallStack) => UnorderedM i a -> AnnotatedElement i -> Parser i (AnnotatedElement i, a)
parseUnorderedElementKeepLeftovers (UnorderedM p) el = swap <$> runStateT p el

-- | Parse an 'unordered' element (corresponding to an xml 'all'). Don't
-- complain about any unparsed nodes or attributes.
parseUnorderedElementPartially :: (HasCallStack) => UnorderedM i a -> AnnotatedElement i -> Parser i a
parseUnorderedElementPartially p el = snd <$> parseUnorderedElementKeepLeftovers p el

consumeOptionalAttribute :: (HasCallStack, FromContent a) => Name -> UnorderedM i (Maybe a)
consumeOptionalAttribute name = do
  el@Element {attributes} <- get
  case Map.lookup name attributes of
    Nothing -> pure Nothing
    Just (text, i) -> do
      a <- UnorderedM . lift $ fromContent text i
      put $ el {attributes = Map.delete name attributes}
      pure $ Just a

consumeAttribute :: (HasCallStack, FromContent a) => Name -> UnorderedM i a
consumeAttribute name = do
  ma <- consumeOptionalAttribute name
  case ma of
    Just a -> pure a
    Nothing -> do
      Element {info} <- get
      throwError . parserError info $ "Missing attribute: " <> renderName name

consumeElementOrAbsent :: (HasCallStack, FromElement a) => Name -> UnorderedM i (Maybe a)
consumeElementOrAbsent name = do
  remaining@Element {children} <- get
  let go _ [] = pure Nothing
      go skipped (node : rest) = case node of
        NodeElement name' el _
          | name == name' -> do
              a <- UnorderedM . lift $ fromElement el
              put $ remaining {children = reverse skipped <> rest}
              pure $ Just a
        _ -> go (node : skipped) rest
  go [] children

consumeElement :: (HasCallStack, FromElement a) => Name -> UnorderedM i a
consumeElement name = do
  ma <- consumeElementOrAbsent name
  case ma of
    Just a -> pure a
    Nothing -> do
      Element {info} <- get
      throwError . parserError info $ "Missing " <> renderName name <> " element."

-- | Parses `Maybe a` via `MaybeEmpty a`.
consumeElementOrEmpty :: (HasCallStack, FromElement a) => Name -> UnorderedM i (Maybe a)
consumeElementOrEmpty name = unMaybeEmpty <$> consumeElement name

consumeElements :: (HasCallStack, FromElement a) => Name -> UnorderedM i [a]
consumeElements name = do
  ma <- consumeElementOrAbsent name
  case ma of
    Nothing -> pure []
    Just a -> (a :) <$> consumeElements name

consumeRemainingElements :: (HasCallStack) => UnorderedM i [(Name, AnnotatedElement i)]
consumeRemainingElements = do
  remaining@Element {children} <- get
  let (rest, res) = go [] children
  put $ remaining {children = reverse rest}
  pure res
  where
    go skipped [] = (skipped, [])
    go skipped (node : rest) = case node of
      NodeElement name el _ ->
        let (skipped', result) = go skipped rest
         in (skipped', (name, el) : result)
      _ -> go (node : skipped) rest

consumeLeftovers :: UnorderedM i Leftovers
consumeLeftovers = do
  remaining <- get
  put remaining {children = mempty, attributes = mempty}
  pure . Leftovers $ unAnnotateElement remaining
