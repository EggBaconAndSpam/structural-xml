module Data.XML.Generic.GenericUnparseElement where

import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import Data.XML
import Data.XML.Generic.Names
import qualified GHC.Generics as GHC
import GHC.TypeLits
import Generics.SOP.BasicFunctors
import Generics.SOP.GGP
import Generics.SOP.NP
import Generics.SOP.NS
import Generics.SOP.Type.Metadata

genericUnparseElement ::
  forall a.
  (GHC.Generic a, GFrom a, GenericUnparseElement a (GDatatypeInfoOf a) (GCode a)) =>
  a ->
  Element
genericUnparseElement x = genericUnparseElement' @a @(GDatatypeInfoOf a) @(GCode a) $ gfrom x

class GenericUnparseElement (a :: Type) (info :: DatatypeInfo) (code :: [[Type]]) where
  genericUnparseElement' :: SOP I code -> Element

class GenericUnparseElement' (a :: Type) (constructors :: [ConstructorInfo]) (code :: [[Type]]) where
  genericUnparseElement'' :: NS (NP I) code -> Element

class GenericUnparseElement'' (a :: Type) (fields :: [FieldInfo]) (code :: [Type]) where
  genericUnparseElement''' :: NP I code -> Element

class GenericUnparseField (a :: Type) (field :: FieldInfo) (l :: LabelClass) (code :: Type) where
  genericUnparseField :: code -> Element

instance
  (GenericUnparseElement' a constructors code) =>
  GenericUnparseElement a ('ADT _m _d constructors _s) code
  where
  genericUnparseElement' (SOP x) = genericUnparseElement'' @a @constructors @code x

instance GenericUnparseElement' a '[] '[] where
  genericUnparseElement'' x = case x of {}

instance
  ( GenericUnparseElement' a constructors codes,
    GenericUnparseElement'' a fields code
  ) =>
  GenericUnparseElement' a ('Record _c fields ': constructors) (code ': codes)
  where
  genericUnparseElement'' x = case x of
    Z y -> genericUnparseElement''' @a @fields @code y
    S y -> genericUnparseElement'' @a @constructors @codes y

instance GenericUnparseElement'' a '[] '[] where
  genericUnparseElement''' Nil = mempty

instance
  ( GenericUnparseField a field (ClassifyFieldLabel field) code,
    GenericUnparseElement'' a fields codes
  ) =>
  GenericUnparseElement'' a (field ': fields) (code ': codes)
  where
  genericUnparseElement''' (I x :* xs) =
    genericUnparseField @a @field @(ClassifyFieldLabel field) @code x
      <> genericUnparseElement''' @a @fields @codes xs

instance
  (MapNamesToXML a, ToContent code, KnownSymbol field) =>
  GenericUnparseField a ('FieldInfo field) 'AttributeClass (Maybe code)
  where
  genericUnparseField Nothing = mempty
  genericUnparseField (Just code) =
    mempty
      { attributes = Map.singleton (mapNameToAttribute @a @field) (toContent code, ())
      }

instance
  {-# OVERLAPPABLE #-}
  (MapNamesToXML a, ToContent code, KnownSymbol field) =>
  GenericUnparseField a ('FieldInfo field) 'AttributeClass code
  where
  genericUnparseField code =
    mempty
      { attributes = Map.singleton (mapNameToAttribute @a @field) (toContent code, ())
      }

instance
  {-# OVERLAPPABLE #-}
  (MapNamesToXML a, ToElement code, KnownSymbol field) =>
  GenericUnparseField a ('FieldInfo field) 'ElementClass code
  where
  genericUnparseField code =
    mempty
      { children = [NodeElement (mapNameToElement @a @field) (toElement code) ()]
      }

instance
  (ToContent code) =>
  GenericUnparseField a ('FieldInfo field) 'ContentClass code
  where
  genericUnparseField code =
    mempty
      { children = [NodeContent (toContent code) ()]
      }

instance
  (MapNamesToXML a, ToElement code, KnownSymbol field) =>
  GenericUnparseField a ('FieldInfo field) 'ElementClass (Maybe code)
  where
  genericUnparseField Nothing = mempty
  genericUnparseField (Just code) =
    mempty
      { children =
          [NodeElement (mapNameToElement @a @field) (toElement code) ()]
      }

instance
  (MapNamesToXML a, ToElement code, KnownSymbol field) =>
  GenericUnparseField a ('FieldInfo field) 'ElementClass (NonEmpty code)
  where
  genericUnparseField (x :| xs) =
    mempty
      { children =
          map
            (\code -> NodeElement (mapNameToElement @a @field) (toElement code) ())
            (x : xs)
      }

instance
  (MapNamesToXML a, ToElement code, KnownSymbol field) =>
  GenericUnparseField a ('FieldInfo field) 'ElementClass [code]
  where
  genericUnparseField xs =
    mempty
      { children =
          map
            (\code -> NodeElement (mapNameToElement @a @field) (toElement code) ())
            xs
      }
