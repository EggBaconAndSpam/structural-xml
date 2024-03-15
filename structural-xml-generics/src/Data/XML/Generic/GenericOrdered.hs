module Data.XML.Generic.GenericOrdered where

import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import Data.XML
import Data.XML.Generic.GenericUnparseElement
import Data.XML.Generic.Names
import Data.XML.Parse.Ordered
import qualified GHC.Generics as GHC
import GHC.TypeLits
import Generics.SOP.BasicFunctors
import Generics.SOP.GGP
import Generics.SOP.NP
import Generics.SOP.NS
import Generics.SOP.Type.Metadata

newtype GenericOrdered a = GenericOrdered a

instance
  (GHC.Generic a, GFrom a, GenericUnparseElement a (GDatatypeInfoOf a) (GCode a)) =>
  ToElement (GenericOrdered a)
  where
  toElement (GenericOrdered a) = genericUnparseElement a

instance
  (GHC.Generic a, GTo a, GenericParseOrdered a (GDatatypeInfoOf a) (GCode a)) =>
  FromElement (GenericOrdered a)
  where
  fromElement el = GenericOrdered <$> genericParseOrdered el

genericParseOrdered ::
  forall a i.
  (GHC.Generic a, GTo a, GenericParseOrdered a (GDatatypeInfoOf a) (GCode a)) =>
  AnnotatedElement i ->
  Parser i a
genericParseOrdered el = gto <$> genericParseOrdered' @a @(GDatatypeInfoOf a) @(GCode a) el

class GenericParseOrdered (a :: Type) (info :: DatatypeInfo) (code :: [[Type]]) where
  genericParseOrdered' :: AnnotatedElement i -> Parser i (SOP I code)

class GenericParseOrdered' (a :: Type) (constructors :: [ConstructorInfo]) (code :: [[Type]]) where
  genericParseOrdered'' :: AnnotatedElement i -> Parser i (NS (NP I) code)

class GenericParseOrdered'' (a :: Type) (fields :: [FieldInfo]) (code :: [Type]) where
  genericParseOrdered''' :: OrderedM i (NP I code)

class GenericParseOrderedField (a :: Type) (field :: FieldInfo) (l :: LabelClass) (code :: Type) where
  genericParseOrderedField :: OrderedM i code

instance
  ( GenericParseOrdered' a constructors codes
  ) =>
  GenericParseOrdered a ('ADT _m _d constructors _s) codes
  where
  genericParseOrdered' el = SOP <$> genericParseOrdered'' @a @constructors @codes el

instance GenericParseOrdered' a '[] '[] where
  genericParseOrdered'' Element {info} = Left . parserError info $ "No constructor matched."

instance GenericParseOrdered'' a '[] '[] where
  genericParseOrdered''' = pure Nil

instance
  ( GenericParseOrdered' a constructors codes,
    GenericParseOrdered'' a fields code
  ) =>
  GenericParseOrdered' a ('Record _c fields ': constructors) (code ': codes)
  where
  genericParseOrdered'' el =
    orParsers
      (Z <$> parseOrderedElement (genericParseOrdered''' @a @fields @code) el)
      (S <$> genericParseOrdered'' @a @constructors @codes el)

instance
  ( GenericParseOrderedField a field (ClassifyFieldLabel field) code,
    GenericParseOrdered'' a fields codes
  ) =>
  GenericParseOrdered'' (a :: Type) (field ': fields) (code ': codes)
  where
  genericParseOrdered''' = do
    x <- genericParseOrderedField @a @field @(ClassifyFieldLabel field) @code
    rest <- genericParseOrdered''' @a @fields @codes
    pure (I x :* rest)

instance
  ( MapNamesToXML a,
    FromElement code,
    KnownSymbol field
  ) =>
  GenericParseOrderedField (a :: Type) ('FieldInfo field) 'ElementClass (Maybe code)
  where
  genericParseOrderedField =
    consumeElementOrAbsent (mapNameToElement @a @field)

instance
  {-# OVERLAPPABLE #-}
  ( MapNamesToXML a,
    FromElement code,
    KnownSymbol field
  ) =>
  GenericParseOrderedField (a :: Type) ('FieldInfo field) 'ElementClass code
  where
  genericParseOrderedField =
    consumeElement (mapNameToElement @a @field)

instance
  ( MapNamesToXML a,
    FromElement code,
    KnownSymbol field
  ) =>
  GenericParseOrderedField (a :: Type) ('FieldInfo field) 'ElementClass [code]
  where
  genericParseOrderedField =
    consumeElements (mapNameToElement @a @field)

instance
  ( MapNamesToXML a,
    FromElement code,
    KnownSymbol field
  ) =>
  GenericParseOrderedField (a :: Type) ('FieldInfo field) 'ElementClass (NonEmpty code)
  where
  genericParseOrderedField = do
    x <- consumeElement $ mapNameToElement @a @field
    xs <- consumeElements $ mapNameToElement @a @field
    pure $ x :| xs

instance GenericParseOrderedField (a :: Type) ('FieldInfo _info) 'ElementClass Leftovers where
  genericParseOrderedField = consumeLeftovers

instance
  {-# OVERLAPPABLE #-}
  ( MapNamesToXML a,
    FromContent code,
    KnownSymbol field
  ) =>
  GenericParseOrderedField (a :: Type) ('FieldInfo field) 'AttributeClass code
  where
  genericParseOrderedField = do
    consumeAttribute (mapNameToAttribute @a @field)

instance
  ( MapNamesToXML a,
    FromContent code,
    KnownSymbol field
  ) =>
  GenericParseOrderedField (a :: Type) ('FieldInfo field) 'AttributeClass (Maybe code)
  where
  genericParseOrderedField = do
    consumeOptionalAttribute (mapNameToAttribute @a @field)
