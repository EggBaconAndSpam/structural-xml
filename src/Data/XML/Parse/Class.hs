module Data.XML.Parse.Class where

import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.XML.Helpers.ContentElement
import Data.XML.Parse.Types
import Data.XML.Types (renderName)
import qualified Data.XML.Types as XML
import GHC.Stack
import Text.Read (readMaybe)
import Text.XML (Name)
import Type.Reflection (Typeable, typeRep)

type Parser i a = Either (ParserError i) a

class FromDocument a where
  fromDocument :: HasCallStack => Document i -> Parser i a

fromRootElement :: FromElement b => Name -> (b -> a) -> Document i -> Parser i a
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
  fromElement :: HasCallStack => Element i -> Parser i a

-- | Textual 'content' appears either inside elements, e.g.
--
-- <element>Text</element>
--
-- or as the content of attributes:
--
-- <element attr_name="<attr_content>"/>
class FromContent a where
  fromContent :: HasCallStack => (Text, i) -> Parser i a

readContent :: forall a i. (Read a, Typeable a) => (Text, i) -> Parser i a
readContent (text, i) = case readMaybe $ Text.unpack text of
  Just a -> pure a
  Nothing -> do
    let typename = Text.pack . show $ typeRep @a
    Left . parserError i $ "Failed to read \"" <> text <> "\" as " <> typename

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
  fromChoiceElement :: Map Name (Element i -> Parser i a)

{- Instances -}
instance FromContent Text where
  fromContent = pure . fst

instance FromContent Int where
  fromContent = readContent

instance FromElement XML.Element where
  fromElement = pure . elementWithoutInfo

instance FromContent a => FromElement (ContentElement a) where
  fromElement el = ContentElement <$> parseContentElement fromContent el

deriving via ContentElement Text instance FromElement Text

deriving via ContentElement Int instance FromElement Int

-- | Expects an element consisting of a single content node or no child nodes
-- (which is treated as an empty content string). Fails if the element isn't
--  fully consumed, i.e. if the element has attributes.
parseContentElement :: ((Text, i) -> Parser i a) -> Element i -> Parser i a
parseContentElement p el@Element {info} = do
  (leftovers, a) <- parseContentElementLax p el
  if isEmptyElement leftovers
    then pure a
    else Left $ parserError info "Failed to parse content element: element not completely consumed (unexpected attributes!)."

-- | Returns leftovers (i.e. attributes).
-- need catch?...
parseContentElementLax ::
  ((Text, i) -> Parser i a) ->
  Element i ->
  Parser i (Element i, a)
parseContentElementLax p el@Element {children, info} = do
  content <- case children of
    [NodeContent c] -> pure c
    [] -> pure ("", info)
    _ -> Left $ parserError info "Failed to parse content element: unexpected child nodes."
  a <- p content
  pure (el {children = []}, a)
