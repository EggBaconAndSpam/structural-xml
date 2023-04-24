module Data.XML
  ( -- * Decoding and rendering
    decodeDocument,
    encodeDocument,

    -- * 'deriving via' helpers for deriving Read and Show
    ReadShowXmlDocument (..),
    ReadShowXmlElement (..),

    -- * Re-exports
    module Data.XML.Types,
    module Data.XML.Parse.Types,
    module Data.XML.Unparse,
  )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import Data.XML.Parse.Types
import Data.XML.Types
import Data.XML.Unparse
import GHC.Stack
import Text.XML (def, parseText, renderText, rsPretty)

decodeDocument :: (HasCallStack, FromDocument a) => Text -> Either String a
decodeDocument raw = case parseText def (Text.Lazy.fromStrict raw) of
  Left err -> Left $ show err
  Right conduitDoc ->
    first prettyParserError . fromDocument . documentWithZipper $
      fromXmlConduit conduitDoc

encodeDocument :: ToDocument a => a -> Text
encodeDocument =
  Text.Lazy.toStrict
    . renderText (def {rsPretty = True})
    . toXmlConduit
    . toDocument

newtype ReadShowXmlDocument a = ReadShowXmlDocument a

instance ToDocument a => Show (ReadShowXmlDocument a) where
  show (ReadShowXmlDocument a) =
    Text.Lazy.unpack . renderText (def {rsPretty = True}) . toXmlConduit $ toDocument a

instance FromDocument a => Read (ReadShowXmlDocument a) where
  readsPrec _ str = case decodeDocument (Text.pack str) of
    Right a -> [(ReadShowXmlDocument a, "")]
    Left err -> error err

newtype ReadShowXmlElement a = ReadShowXmlElement a

instance FromElement a => FromDocument (ReadShowXmlElement a) where
  fromDocument Document {root} = ReadShowXmlElement <$> fromElement root

deriving via ReadShowXmlDocument (ReadShowXmlElement a) instance FromElement a => Read (ReadShowXmlElement a)

instance ToElement a => Show (ReadShowXmlElement a) where
  show (ReadShowXmlElement a) =
    Text.Lazy.unpack . renderText (def {rsPretty = True}) $
      toXmlConduit
        Document {root = toElement a, rootName = "root_element", info = ()}
