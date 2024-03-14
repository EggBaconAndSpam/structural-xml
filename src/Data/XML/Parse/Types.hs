module Data.XML.Parse.Types
  ( Parser,
    orParsers,
    ParserError (..),
    parserError,
    prettyParserError,
    prettyLastParserError,
    prettyParserErrorWithCallStack,
    FromDocument (..),
    fromRootElement,
    FromElement (..),
    AnyElement (..),
    --    FromChoiceElement (..),
    FromContent (..),
    readContent,
    parseContentElement,
    parseContentElementPartially,
    parseContentElementKeepLeftovers,
  )
where

import Control.Exception (Exception)
import Data.Decimal
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Data.Time.Format.ISO8601
import Data.XML.Parse.Location
import Data.XML.Types
import GHC.Stack
import Text.Read (readMaybe)
import Type.Reflection (Typeable, typeRep)

data ParserError i = ParserError
  { callstack :: CallStack,
    info :: i,
    message :: String -- could turn into a sum?
  }
  deriving stock (Show)
  deriving anyclass (Exception)

parserError :: (HasCallStack) => i -> String -> NonEmpty (ParserError i)
parserError info message =
  NonEmpty.singleton ParserError {callstack = callStack, ..}

-- Print only the last parser error, i.e. the one where the parser got the
-- farthest before failing.
prettyLastParserError :: NonEmpty (ParserError Location) -> String
prettyLastParserError =
  prettyParserError . NonEmpty.head . NonEmpty.sortWith (\ParserError {info} -> info)

prettyParserError :: ParserError Location -> String
prettyParserError ParserError {..} =
  unlines
    ( message
        : "To get to the error location:"
        : map ("  " <>) (printPath info)
    )

prettyParserErrorWithCallStack :: ParserError Location -> String
prettyParserErrorWithCallStack err@ParserError {callstack} =
  prettyParserError err <> prettyCallStack callstack

type Parser i a = Either (NonEmpty (ParserError i)) a

-- If we newtype wrapped Parser this could be an Alternative instance instead.
orParsers :: Parser i a -> Parser i a -> Parser i a
orParsers p1 p2 = case p1 of
  Right a -> Right a
  Left err1 -> case p2 of
    Right a -> Right a
    Left err2 -> Left (err1 <> err2)

class FromDocument a where
  fromDocument :: (HasCallStack) => AnnotatedDocument i -> Parser i a

fromRootElement :: (HasCallStack, FromElement a) => Name -> AnnotatedDocument i -> Parser i a
fromRootElement name Document {..} =
  if rootName == name
    then fromElement root
    else
      Left . parserError info $
        "Failed to parse document: expected root element with name "
          <> renderName name
          <> " but got "
          <> renderName rootName

class FromElement a where
  fromElement :: (HasCallStack) => AnnotatedElement i -> Parser i a

-- | Textual 'content' appears either inside elements, e.g.
--
-- <element>Text</element>
--
-- or as the content of attributes:
--
-- <element attr_name="<attr_content>"/>
class FromContent a where
  fromContent :: (HasCallStack) => Text -> i -> Parser i a

readContent :: forall a i. (HasCallStack, Read a, Typeable a) => Text -> i -> Parser i a
readContent text i = case readMaybe $ Text.unpack text of
  Just a -> pure a
  Nothing -> do
    Left . parserError i $
      "Failed to read \""
        <> Text.unpack text
        <> "\" as "
        <> show (typeRep @a)

instance (FromElement a) => FromElement (MaybeEmpty a) where
  fromElement el =
    if isEmptyElement el
      then pure $ MaybeEmpty Nothing
      else MaybeEmpty . Just <$> fromElement el

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

{-
class FromChoiceElement a where
  -- separate FromOrderedChoiceElement, FromUnorderedChoiceElement
  -- HasCallStack => OrderedM a
  -- better: only in OrderedM (not as useful perhaps in UnorderedM)
  fromChoiceElement :: HasCallStack => Map Name (AnnotatedElement i -> Parser i a)
-}
{- Instances -}
instance FromContent Text where
  fromContent t _ = pure t

instance FromContent Int where
  fromContent = readContent

instance FromContent UTCTime where
  fromContent t i = case iso8601ParseM $ Text.unpack t of
    Nothing -> Left . parserError i $ "Not a valid timestamp in ISO8601 format: " <> Text.unpack t
    Just u -> pure $ zonedTimeToUTC u

deriving via ContentElement UTCTime instance FromElement UTCTime

instance FromContent Day where
  fromContent = readContent

deriving via ContentElement Day instance FromElement Day

instance FromContent Bool where
  fromContent "true" _ = pure True
  fromContent "false" _ = pure False
  fromContent t i =
    Left . parserError i $
      "Failed to read \""
        <> Text.unpack t
        <> "\" as boolean"

deriving via ContentElement Bool instance FromElement Bool

instance FromElement Element where
  fromElement = pure . unAnnotateElement

instance FromDocument Document where
  fromDocument = pure . unAnnotateDocument

instance (FromContent a) => FromElement (ContentElement a) where
  fromElement el = ContentElement <$> parseContentElement fromContent el

-- todo: custom type, move somewhere else
instance FromContent Decimal where
  fromContent = readContent

deriving via ContentElement Decimal instance FromElement Decimal

deriving via ContentElement Text instance FromElement Text

deriving via ContentElement Int instance FromElement Int

-- | Expects an element consisting of a single content node or no child nodes
-- (which is treated as an empty content string). Fails if the element isn't
--  fully consumed, e.g. if the element has attributes.
parseContentElement ::
  (HasCallStack) => (Text -> i -> Parser i a) -> AnnotatedElement i -> Parser i a
parseContentElement p el@Element {info} = do
  (leftovers, a) <- parseContentElementKeepLeftovers p el
  if isEmptyElement leftovers
    then pure a
    else Left $ parserError info "Failed to parse content element: element not completely consumed (unexpected attributes!)."

-- | Returns the leftovers (i.e. any attributes).
parseContentElementKeepLeftovers ::
  (HasCallStack) =>
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
  (HasCallStack) =>
  (Text -> i -> Parser i a) ->
  AnnotatedElement i ->
  Parser i a
parseContentElementPartially p el =
  snd <$> parseContentElementKeepLeftovers p el

newtype AnyElement = AnyElement (Name, Element)
  deriving stock (Eq, Ord, Show)
