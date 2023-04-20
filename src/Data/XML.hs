module Data.XML
  ( module Data.XML.Types,
    module Data.XML.Serialisation,
    module Data.XML.Deserialisation,
    parseDocument,
    parseDocument',
    unsafeParseDocument,
    unparseDocument,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text (fromStrict, toStrict)
import Data.XML.Deserialisation
import Data.XML.Serialisation
import Data.XML.Types
import qualified Text.XML as XC

parseDocument :: FromDocument a => Text -> Either ParserError a
parseDocument xml = do
  conduitDoc <-
    either (throwParserError . Text.pack . show) pure $
      XC.parseText XC.def (Text.fromStrict xml)
  fromDocument (fromXmlConduit conduitDoc)

parseDocument' :: FromDocument a => Text -> Maybe a
parseDocument' = either (const Nothing) Just . parseDocument

unsafeParseDocument :: FromDocument a => Text -> a
unsafeParseDocument = either undefined id . parseDocument

unparseDocument :: ToDocument a => a -> Text
unparseDocument = Text.toStrict . XC.renderText XC.def . toXmlConduit . toDocument
