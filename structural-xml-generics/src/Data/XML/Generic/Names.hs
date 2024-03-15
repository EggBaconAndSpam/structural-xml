{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.XML.Generic.Names
  ( mapNameToElement,
    mapNameToAttribute,
    mapNameToEnum,
    MapNamesToXML (..),
    ClassifyContentLabel,
    ClassifyFieldLabel,
    LabelClass (..),
  )
where

import Data.List (stripPrefix)
import Data.Maybe
import Data.Proxy
import Data.String (fromString)
import Data.XML (Name)
import GHC.TypeLits
import Generics.SOP.Type.Metadata

data LabelClass = AttributeClass | ElementClass | ContentClass

stripElementPrefix :: String -> String
stripElementPrefix s = fromMaybe s $ stripPrefix "elem_" s

stripAttributePrefix :: String -> String
stripAttributePrefix s = fromMaybe s $ stripPrefix "attr_" s

type family ClassifyContentLabel (info :: FieldInfo) :: LabelClass where
  ClassifyContentLabel ('FieldInfo label) = ClassifyContentLabel' (IsPrefix "attr_" label)

type family
  ClassifyContentLabel' (isAttr :: Bool) ::
    LabelClass
  where
  ClassifyContentLabel' 'True = 'AttributeClass
  ClassifyContentLabel' 'False = 'ContentClass

type family ClassifyFieldLabel (info :: FieldInfo) :: LabelClass where
  ClassifyFieldLabel ('FieldInfo label) =
    ClassifyFieldLabel'
      (IsPrefix "attr_" label)
      (IsPrefix "elem_" label)

type family
  ClassifyFieldLabel'
    (isAttr :: Bool)
    (isElement :: Bool) ::
    LabelClass
  where
  ClassifyFieldLabel' 'True 'False = 'AttributeClass
  ClassifyFieldLabel' 'False _ = 'ElementClass

type family IsPrefix (a :: Symbol) (b :: Symbol) :: Bool where
  IsPrefix a b = IsPrefix' (CmpSymbol a b) (CmpSymbol (AppendSymbol a Terminator) b)

-- The 'terminator' is the last valid unicode character (that is also guaranteed
-- to never be used for encoding, i.e. it will never appear in a Haskell name).
-- We use that fact that `a` is a prefix of `b` iff `a < b` and `(a ++
-- Terminator) > b`.
type Terminator = "\x10FFFF" :: Symbol

type family IsPrefix' (l :: Ordering) (r :: Ordering) :: Bool where
  IsPrefix' 'LT 'GT = 'True
  IsPrefix' 'EQ 'GT = 'True
  IsPrefix' _ _ = 'False

class MapNamesToXML a where
  mapNamesToElements :: String -> Name
  mapNamesToAttributes :: String -> Name
  mapNamesToEnum :: String -> String
  mapNamesToElements = fromString
  mapNamesToAttributes = fromString
  mapNamesToEnum = id

mapNameToElement :: forall a label. (MapNamesToXML a, KnownSymbol label) => Name
mapNameToElement = mapNamesToElements @a . stripElementPrefix . symbolVal $ Proxy @label

mapNameToAttribute :: forall a label. (MapNamesToXML a, KnownSymbol label) => Name
mapNameToAttribute = mapNamesToAttributes @a . stripAttributePrefix . symbolVal $ Proxy @label

mapNameToEnum :: forall a label. (MapNamesToXML a, KnownSymbol label) => String
mapNameToEnum = mapNamesToEnum @a . symbolVal $ Proxy @label
