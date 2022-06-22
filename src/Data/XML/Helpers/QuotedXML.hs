module Data.XML.Helpers.QuotedXML (QuotedXml (..)) where

import Control.Monad.Error.Class (liftEither)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text (fromStrict, toStrict)
import Data.XML.Deserialisation
import Data.XML.Helpers.ContentElement
import Data.XML.Serialisation
import Data.XML.Types
import GHC.Generics
import qualified Text.XML as XC

-- | A quoted xml document inside an xml element, e.g.
--
-- <QuotedXml><![CDATA[<?xml version="1.0" encoding="iso-8859-1" standalone="yes"?>
-- <soapenv:Envelope xmlns:soapenv='http://schemas.xmlsoap.org/soap/envelope/' xmlns:tem='http://tempuri.org/'>
--  <soapenv:Header />
--  <soapenv:Body>
--    ...
--  </soapenv:Body>
-- </soapenv:Envelope>
-- ]]>
-- </QuotedXml>
newtype QuotedXml a = QuotedXml {unQuoted :: a}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show)

instance FromDocument a => FromElement (QuotedXml a) where
  fromElement = parseContentElement $ \text ->
    case XC.parseText XC.def (Text.fromStrict text) of
      Left err -> throwParserError . Text.pack $ show err
      Right doc -> QuotedXml <$> liftEither (fromDocument $ fromXmlConduit doc)

instance ToDocument a => ToElement (QuotedXml a) where
  toElement (QuotedXml a) =
    let text = Text.toStrict . XC.renderText XC.def $ toXmlConduit (toDocument a)
     in toElement (ContentElement text)
