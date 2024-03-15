{-# LANGUAGE MonoLocalBinds #-}

module Data.XML.ISO20022.DerivingViaHelpers where

import Data.String (IsString (..))
import Data.XML
import Data.XML.Generic
import Data.XML.ISO20022.NameMangling
import GHC.Generics (Generic)

newtype Camt53Document a = Camt53Document a
  deriving newtype (Generic)

instance MapNamesToXML (Camt53Document a) where
  mapNamesToElements l =
    (fromString l)
      { nameNamespace = Just "urn:iso:std:iso:20022:tech:xsd:camt.053.001.02"
      }

instance
  (FromDocument (GenericDocument (Camt53Document a))) =>
  FromDocument (Camt53Document a)
  where
  fromDocument el = (\(GenericDocument a) -> a) <$> fromDocument el

instance
  (ToDocument (GenericDocument (Camt53Document a))) =>
  ToDocument (Camt53Document a)
  where
  toDocument = toDocument . GenericDocument

newtype Camt53Element a = Camt53Element a
  deriving newtype (Generic)

instance MapNamesToXML (Camt53Element a) where
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
  (FromElement (GenericOrdered (Camt53Element a))) =>
  FromElement (Camt53Element a)
  where
  fromElement el = (\(GenericOrdered a) -> a) <$> fromElement el

instance
  (ToElement (GenericOrdered (Camt53Element a))) =>
  ToElement (Camt53Element a)
  where
  toElement = toElement . GenericOrdered

newtype Camt53Enum a = Camt53Enum a
  deriving (MapNamesToXML) via Camt53Element a
  deriving newtype (Generic)

instance
  (FromContent (GenericEnum (Camt53Enum a))) =>
  FromContent (Camt53Enum a)
  where
  fromContent t i = (\(GenericEnum a) -> a) <$> fromContent t i

instance
  (ToContent (GenericEnum (Camt53Enum a))) =>
  ToContent (Camt53Enum a)
  where
  toContent = toContent . GenericEnum

deriving via
  ContentElement (Camt53Enum a)
  instance
    (FromContent (Camt53Enum a)) => FromElement (Camt53Enum a)

deriving via
  ContentElement (Camt53Enum a)
  instance
    (ToContent (Camt53Enum a)) => ToElement (Camt53Enum a)

newtype Camt53Content a = Camt53Content a
  deriving (MapNamesToXML) via Camt53Element a
  deriving newtype (Generic)

instance
  (FromElement (GenericContent (Camt53Content a))) =>
  FromElement (Camt53Content a)
  where
  fromElement el = (\(GenericContent a) -> a) <$> fromElement el

instance
  (ToElement (GenericContent (Camt53Content a))) =>
  ToElement (Camt53Content a)
  where
  toElement = toElement . GenericContent
