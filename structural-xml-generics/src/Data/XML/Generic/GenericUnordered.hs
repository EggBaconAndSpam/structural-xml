module Data.XML.Generic.GenericUnordered where

import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import Data.XML
import Data.XML.Generic.GenericUnparseElement
import Data.XML.Generic.Names
import Data.XML.Parse.Unordered
import qualified GHC.Generics as GHC
import GHC.TypeLits
import Generics.SOP.BasicFunctors
import Generics.SOP.GGP
import Generics.SOP.NP
import Generics.SOP.NS
import Generics.SOP.Type.Metadata

newtype GenericUnordered a = GenericUnordered a

instance
  (GHC.Generic a, GTo a, GenericParseUnordered a (GDatatypeInfoOf a) (GCode a)) =>
  FromElement (GenericUnordered a)
  where
  fromElement el = GenericUnordered <$> genericParseUnordered el

instance
  (GHC.Generic a, GFrom a, GenericUnparseElement a (GDatatypeInfoOf a) (GCode a)) =>
  ToElement (GenericUnordered a)
  where
  toElement (GenericUnordered x) = genericUnparseElement x

genericParseUnordered ::
  forall a i.
  (GHC.Generic a, GTo a, GenericParseUnordered a (GDatatypeInfoOf a) (GCode a)) =>
  AnnotatedElement i ->
  Parser i a
genericParseUnordered el = gto <$> genericParseUnordered' @a @(GDatatypeInfoOf a) @(GCode a) el

class GenericParseUnordered (a :: Type) (info :: DatatypeInfo) (code :: [[Type]]) where
  genericParseUnordered' :: AnnotatedElement i -> Parser i (SOP I code)

class GenericParseUnordered' (a :: Type) (constructors :: [ConstructorInfo]) (code :: [[Type]]) where
  genericParseUnordered'' :: AnnotatedElement i -> Parser i (NS (NP I) code)

class GenericParseUnordered'' (a :: Type) (fields :: [FieldInfo]) (code :: [Type]) where
  genericParseUnordered''' :: UnorderedM i (NP I code)

class GenericParseUnorderedField (a :: Type) (field :: FieldInfo) (l :: LabelClass) (code :: Type) where
  genericParseUnorderedField :: UnorderedM i code

instance
  (GenericParseUnordered' a constructors codes) =>
  GenericParseUnordered a ('ADT _m _d constructors _s) codes
  where
  genericParseUnordered' el = SOP <$> genericParseUnordered'' @a @constructors @codes el

instance GenericParseUnordered' (a :: Type) '[] '[] where
  genericParseUnordered'' Element {info} = Left . parserError info $ "No constructor matched."

instance GenericParseUnordered'' a '[] '[] where
  genericParseUnordered''' = pure Nil

instance
  ( GenericParseUnordered' a constructors codes,
    GenericParseUnordered'' a fields code
  ) =>
  GenericParseUnordered' a ('Record _c fields ': constructors) (code ': codes)
  where
  genericParseUnordered'' el =
    orParsers
      (Z <$> parseUnorderedElement (genericParseUnordered''' @a @fields @code) el)
      (S <$> genericParseUnordered'' @a @constructors @codes el)

instance
  ( GenericParseUnorderedField a field (ClassifyFieldLabel field) code,
    GenericParseUnordered'' a fields codes
  ) =>
  GenericParseUnordered'' (a :: Type) (field ': fields) (code ': codes)
  where
  genericParseUnordered''' = do
    x <- genericParseUnorderedField @a @field @(ClassifyFieldLabel field) @code
    rest <- genericParseUnordered''' @a @fields @codes
    pure (I x :* rest)

instance
  {-# OVERLAPPING #-}
  ( MapNamesToXML a,
    FromElement code,
    KnownSymbol field
  ) =>
  GenericParseUnorderedField (a :: Type) ('FieldInfo field) 'ElementClass (Maybe code)
  where
  genericParseUnorderedField =
    consumeElementOrAbsent (mapNameToElement @a @field)

instance
  {-# OVERLAPPABLE #-}
  ( MapNamesToXML a,
    FromElement code,
    KnownSymbol field
  ) =>
  GenericParseUnorderedField (a :: Type) ('FieldInfo field) 'ElementClass code
  where
  genericParseUnorderedField =
    consumeElement (mapNameToElement @a @field)

instance
  {-# OVERLAPPING #-}
  ( MapNamesToXML a,
    FromElement code,
    KnownSymbol field
  ) =>
  GenericParseUnorderedField (a :: Type) ('FieldInfo field) 'ElementClass [code]
  where
  genericParseUnorderedField =
    consumeElements (mapNameToElement @a @field)

instance
  {-# OVERLAPPING #-}
  ( MapNamesToXML a,
    FromElement code,
    KnownSymbol field
  ) =>
  GenericParseUnorderedField (a :: Type) ('FieldInfo field) 'ElementClass (NonEmpty code)
  where
  genericParseUnorderedField = do
    x <- consumeElement $ mapNameToElement @a @field
    xs <- consumeElements $ mapNameToElement @a @field
    pure $ x :| xs

instance GenericParseUnorderedField (a :: Type) _info _label Leftovers where
  genericParseUnorderedField = consumeLeftovers

instance
  {-# OVERLAPPABLE #-}
  ( MapNamesToXML a,
    FromContent code,
    KnownSymbol field
  ) =>
  GenericParseUnorderedField (a :: Type) ('FieldInfo field) 'AttributeClass code
  where
  genericParseUnorderedField = do
    consumeAttribute (mapNameToAttribute @a @field)

instance
  {-# OVERLAPPING #-}
  ( MapNamesToXML a,
    FromContent code,
    KnownSymbol field
  ) =>
  GenericParseUnorderedField (a :: Type) ('FieldInfo field) 'AttributeClass (Maybe code)
  where
  genericParseUnorderedField = do
    consumeOptionalAttribute (mapNameToAttribute @a @field)
