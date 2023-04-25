{-# LANGUAGE EmptyCase #-}

module Data.XML.Generics.NamedSum where

import Data.Bifunctor (second)
import Data.Kind
import Data.Proxy
import Data.String (IsString (..))
import Data.XML.Deserialisation
import GHC.TypeLits

-- Sum Option? "sum_".
data NamedSum (xs :: [(Symbol, Type)]) where
  Z :: forall x label xs. x -> NamedSum ('(label, x) ': xs)
  S :: forall x xs. NamedSum xs -> NamedSum (x ': xs)

instance Show (NamedSum '[]) where
  show = \case

instance (KnownSymbol label, Show (NamedSum xs), Show x) => Show (NamedSum ('(label, x) ': xs)) where
  show (S xs) = show xs
  show (Z x) = symbolVal (Proxy @label) ++ ": " ++ show x

instance FromChoiceElement (NamedSum '[]) where
  fromElementChoice = []

instance
  (KnownSymbol label, FromElement x, FromChoiceElement (NamedSum xs)) =>
  FromChoiceElement (NamedSum ('(label, x) ': xs))
  where
  fromElementChoice =
    (fromString $ symbolVal (Proxy @label), fmap Z . fromElement) :
    map (second (fmap S .)) (fromElementChoice @(NamedSum xs))
