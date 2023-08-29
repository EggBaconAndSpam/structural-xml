module Data.XML.Unparse
  ( -- * classes
    ToDocument (..),
    toRootElement,
    ToElement (..),
    ToContent (..),
    ToChoiceElement (..),

    -- * A monad for constructing elements
    ConstructM (..),
    constructElement,

    -- * combinators
    appendContent,
    appendElement,
    appendElementOrEmpty,
    appendChoiceElement,
    addAttribute,
    module Text.XML,
  )
where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.XML.Types
import Text.XML (Name)

{- Classes -}

class ToDocument a where
  toDocument :: a -> Document

instance ToDocument (AnnotatedDocument i) where
  toDocument = unAnnotateDocument

toRootElement :: ToElement a => Name -> a -> Document
toRootElement name a = Document {rootName = name, root = toElement a, info = ()}

class ToElement a where
  toElement :: a -> Element

instance ToElement (AnnotatedElement i) where
  toElement = unAnnotateElement

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
  toChoiceElement :: a -> (Name, Element)

{- Constructing elements -}
-- Note that the Element state has its child nodes in reverse order!
newtype ConstructM a = ConstructM (State Element a)
  deriving newtype (Functor, Applicative, Monad, MonadState Element)

-- | Construct an element from the empty element by appending child elements,
-- content and attributes.
constructElement :: ConstructM () -> Element
constructElement (ConstructM c) =
  let Element {..} = execState c emptyElement
   in Element {children = reverse children, ..}

appendContent :: Text -> ConstructM ()
appendContent text = ConstructM . modify $ \Element {..} ->
  Element {children = NodeContent text () : children, ..}

appendElement :: ToElement a => Name -> a -> ConstructM ()
appendElement name a = ConstructM . modify $ \Element {..} ->
  Element {children = NodeElement name (toElement a) () : children, ..}

appendElementOrEmpty :: ToElement a => Name -> Maybe a -> ConstructM ()
appendElementOrEmpty name ma = ConstructM . modify $ \Element {..} ->
  Element {children = NodeElement name (maybe emptyElement toElement ma) () : children, ..}

addAttribute :: ToContent a => Name -> a -> ConstructM ()
addAttribute name a = ConstructM . modify $ \Element {..} ->
  Element {attributes = Map.insert name (toContent a, ()) attributes, ..}

appendChoiceElement :: ToChoiceElement a => a -> ConstructM ()
appendChoiceElement a = ConstructM . modify $ \Element {..} ->
  Element {children = NodeElement name el () : children, ..}
  where
    (name, el) = toChoiceElement a

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
        children = [NodeContent (toContent a) ()],
        info = ()
      }

deriving via ContentElement Text instance ToElement Text

deriving via ContentElement Int instance ToElement Int
