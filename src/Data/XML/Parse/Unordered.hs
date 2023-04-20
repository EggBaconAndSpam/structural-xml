module Data.XML.Parse.Unordered
  ( parseUnorderedElement,
    parseUnorderedElementLax,
    consumeAttribute,
    consumeOptionalAttribute,
    consumeElement,
    consumeElementOrAbsent,
    consumeElementOrEmpty,
    consumeElements,
    consumeChoiceElement,
  )
where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Tuple
import Data.XML.Parse.Types
import Data.XML.Types
import Text.XML (Name)

-- Quadratic behaviour in number of children. Could be improved, but probably not an issue.
newtype UnorderedM i a = UnorderedM (StateT (AnnotatedElement i) (Either (ParserError i)) a)
  deriving newtype (Functor, Applicative, Monad, MonadError (ParserError i), MonadState (AnnotatedElement i))

-- | Parse an 'ordered' element (corresponding to an xml 'sequence'). Fails if
-- the element is not fully consumed.
parseUnorderedElement :: UnorderedM i a -> AnnotatedElement i -> Parser i a
parseUnorderedElement p el@Element {info} = do
  (el', a) <- parseUnorderedElementLax p el
  if isEmptyElement el'
    then pure a
    else throwError $ parserError info "Element not fully consumed!" -- todo: include unparsed remainder!

-- | Parse an 'unordered' element (corresponding to an xml 'all'). Return the
-- rest of the element that wasn't consumed.
parseUnorderedElementLax :: UnorderedM i a -> AnnotatedElement i -> Parser i (AnnotatedElement i, a)
parseUnorderedElementLax (UnorderedM p) el = swap <$> runStateT p el

consumeOptionalAttribute :: FromContent a => Name -> UnorderedM i (Maybe a)
consumeOptionalAttribute name = do
  el@Element {attributes} <- get
  case Map.lookup name attributes of
    Nothing -> pure Nothing
    Just (text, i) -> do
      a <- UnorderedM . lift $ fromContent text i
      put $ el {attributes = Map.delete name attributes}
      pure $ Just a

consumeAttribute :: FromContent a => Name -> UnorderedM i a
consumeAttribute name = do
  ma <- consumeOptionalAttribute name
  case ma of
    Just a -> pure a
    Nothing -> do
      Element {info} <- get
      throwError . parserError info $ "Missing attribute: " <> renderName name

consumeElementOrAbsent :: FromElement a => Name -> UnorderedM i (Maybe a)
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

consumeElement :: FromElement a => Name -> UnorderedM i a
consumeElement name = do
  ma <- consumeElementOrAbsent name
  case ma of
    Just a -> pure a
    Nothing -> do
      Element {info} <- get
      throwError . parserError info $ "Missing " <> renderName name <> " element."

-- | Parses `Maybe a` via `OrEmpty a`.
consumeElementOrEmpty :: FromElement a => Name -> UnorderedM i (Maybe a)
consumeElementOrEmpty name = unOrEmpty <$> consumeElement name

consumeElements :: (FromElement a, Eq i) => Name -> UnorderedM i [a]
consumeElements name = do
  ma <- consumeElementOrAbsent name
  case ma of
    Nothing -> pure []
    Just a -> (a :) <$> consumeElements name

consumeChoiceElement :: forall a i. FromChoiceElement a => UnorderedM i a
consumeChoiceElement = do
  remaining@Element {children, info} <- get
  let go _ [] =
        throwError . parserError info $
          "Missing choice node (one of): "
            <> (intercalate ", " . map renderName . Map.keys $ fromChoiceElement @a)
      go skipped (node : rest) = case node of
        NodeElement name el _
          | Just p <- Map.lookup name fromChoiceElement -> do
              a <- UnorderedM . lift $ p el
              put $ remaining {children = reverse skipped <> rest}
              pure a
        _ -> go (node : skipped) rest
  go [] children
