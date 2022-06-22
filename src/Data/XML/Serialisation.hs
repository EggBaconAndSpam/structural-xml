module Data.XML.Serialisation
  ( ToDocument (..),
    ToElement (..),
    ToContent (..),
    ToChoiceElement (..),
    constructElement,
    appendContent,
    appendElement,
    appendElementOrEmpty,
    addAttribute,
    appendMergeElement,
  )
where

import Control.Monad.Writer.Strict
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.XML.Helpers.ContentElement
import Data.XML.Types
import Text.XML (Name)

{- Classes -}

class ToDocument a where
  toDocument :: a -> Document

class ToElement a where
  toElement :: a -> Element

-- | Textual 'content' appears either inside elements, e.g.
--
-- <element>Text</element>
--
-- or as the content of attributes:
--
-- <element attr_name="<attr_content>"/>
class ToContent a where
  toContent :: a -> Text

-- |
-- A 'choice' element corresponds to the 'choice' schema element, which encodes sum types:
--
-- <example>
--   <A>... Something ...</A>
-- </example>
--
-- and
--
-- <example>
--   <B>... SomethingElse ...</B>
-- </example>
--
-- might both be instances of the sum type represented in Haskell as
--
-- data Example = A Something | B SomethingElse
class ToChoiceElement a where
  toElementChoice :: a -> (Name, Element)

{- Constructing elements -}

data Instruction = AppendElement Name Element | AppendContent Text | AddAttribute Name Text

-- | Construct an element from the empty element by appending child elements,
-- content and attributes.
constructElement :: Writer [Instruction] () -> Element
constructElement c =
  let instructions = execWriter c
      children =
        mapMaybe
          ( \case
              AppendElement name el -> Just (NodeElement name el)
              AppendContent text -> Just (NodeContent text)
              AddAttribute _ _ -> Nothing
          )
          instructions
      attributes =
        Map.fromList $
          mapMaybe
            ( \case
                AppendElement _ _ -> Nothing
                AppendContent _ -> Nothing
                AddAttribute name text -> Just (name, text)
            )
            instructions
   in Element {..}

appendContent :: Text -> Writer [Instruction] ()
appendContent text = tell [AppendContent text]

appendElement :: ToElement a => Name -> a -> Writer [Instruction] ()
appendElement name a = tell [AppendElement name (toElement a)]

appendElementOrEmpty :: ToElement a => Name -> Maybe a -> Writer [Instruction] ()
appendElementOrEmpty name Nothing = appendElement name emptyElement
appendElementOrEmpty name (Just a) = appendElement name a

addAttribute :: ToContent a => Name -> a -> Writer [Instruction] ()
addAttribute name a = tell [AddAttribute name (toContent a)]

-- For the niche use case of joining two elements. We could also define a Monoid
-- instance for Element.
appendMergeElement :: Element -> Writer [Instruction] ()
appendMergeElement Element {..} = do
  forM_ (Map.toList attributes) $ uncurry addAttribute
  forM_ children $ \case
    NodeElement name element -> appendElement name element
    NodeContent text -> appendContent text

{- Instances -}

instance ToContent Text where
  toContent = id

instance ToContent Int where
  toContent = Text.pack . show

instance ToElement Element where
  toElement = id

instance ToContent a => ToElement (ContentElement a) where
  toElement (ContentElement a) =
    Element
      { attributes = Map.empty,
        children = [NodeContent $ toContent a]
      }

deriving via ContentElement Text instance ToElement Text

deriving via ContentElement Int instance ToElement Int
