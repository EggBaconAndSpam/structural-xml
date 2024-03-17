{-# LANGUAGE MonoLocalBinds #-}

module Data.XML.ISO20022.DerivingViaHelpers where

import Data.String (IsString (..))
import Data.XML
import Data.XML.Generic
import Data.XML.ISO20022.NameMangling
import GHC.Generics (Generic)

newtype ISO20022Document a = ISO20022Document a
  deriving newtype (Generic)

instance MapNamesToXML (ISO20022Document a) where
  mapNamesToElements l =
    (fromString l)
      { nameNamespace = Just "urn:iso:std:iso:20022:tech:xsd:camt.053.001.02"
      }

instance
  (FromDocument (GenericDocument (ISO20022Document a))) =>
  FromDocument (ISO20022Document a)
  where
  fromDocument el = (\(GenericDocument a) -> a) <$> fromDocument el

instance
  (ToDocument (GenericDocument (ISO20022Document a))) =>
  ToDocument (ISO20022Document a)
  where
  toDocument = toDocument . GenericDocument

newtype ISO20022Element a = ISO20022Element a
  deriving newtype (Generic)

instance MapNamesToXML (ISO20022Element a) where
  mapNamesToElements l =
    (fromString $ mangleName l)
      { nameNamespace = Just "urn:iso:std:iso:20022:tech:xsd:camt.053.001.02"
      }

  mapNamesToAttributes = fromString . mangleName

  -- Enums have their types prefixed to the constructors to disambiguate them. We
  -- can move them into their own modules to get rid of this.
  mapNamesToEnum s = case dropWhile (/= '_') s of
    '_' : rest -> rest
    _ -> s

instance
  (FromElement (GenericOrdered (ISO20022Element a))) =>
  FromElement (ISO20022Element a)
  where
  fromElement el = (\(GenericOrdered a) -> a) <$> fromElement el

instance
  (ToElement (GenericOrdered (ISO20022Element a))) =>
  ToElement (ISO20022Element a)
  where
  toElement = toElement . GenericOrdered

newtype ISO20022Enum a = ISO20022Enum a
  deriving (MapNamesToXML) via ISO20022Element a
  deriving newtype (Generic)

instance
  (FromContent (GenericEnum (ISO20022Enum a))) =>
  FromContent (ISO20022Enum a)
  where
  fromContent t i = (\(GenericEnum a) -> a) <$> fromContent t i

instance
  (ToContent (GenericEnum (ISO20022Enum a))) =>
  ToContent (ISO20022Enum a)
  where
  toContent = toContent . GenericEnum

deriving via
  ContentElement (ISO20022Enum a)
  instance
    (FromContent (ISO20022Enum a)) => FromElement (ISO20022Enum a)

deriving via
  ContentElement (ISO20022Enum a)
  instance
    (ToContent (ISO20022Enum a)) => ToElement (ISO20022Enum a)

newtype ISO20022Content a = ISO20022Content a
  deriving (MapNamesToXML) via ISO20022Element a
  deriving newtype (Generic)

instance
  (FromElement (GenericContent (ISO20022Content a))) =>
  FromElement (ISO20022Content a)
  where
  fromElement el = (\(GenericContent a) -> a) <$> fromElement el

instance
  (ToElement (GenericContent (ISO20022Content a))) =>
  ToElement (ISO20022Content a)
  where
  toElement = toElement . GenericContent
