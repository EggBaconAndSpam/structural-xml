{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.XML.Parse.Generically.Labelled where

import Control.Monad.Except
import Control.Monad.State
import Data.Kind
import Data.List (stripPrefix)
import Data.List.NonEmpty hiding (map)
import qualified Data.Map as Map
import Data.Maybe
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.XML
import Data.XML.BoundedList
import Data.XML.Parse.Ordered
import qualified Data.XML.Parse.Ordered as Ordered
import Data.XML.Parse.Unordered (UnorderedM, parseUnorderedElement)
import qualified Data.XML.Parse.Unordered as Unordered
import qualified GHC.Generics as GHC
import GHC.TypeLits
import Generics.SOP (NP (..), NS (..), Proxy (..), SOP (..))
import Generics.SOP.BasicFunctors
import Generics.SOP.GGP
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

newtype GenericOrdered a = GenericOrdered a

instance
  (GHC.Generic a, GTo a, GenericParseOrdered a (GDatatypeInfoOf a) (GCode a)) =>
  FromElement (GenericOrdered a)
  where
  fromElement el = GenericOrdered <$> genericParseOrdered el

instance
  (GHC.Generic a, GFrom a, GenericUnparseElement a (GDatatypeInfoOf a) (GCode a)) =>
  ToElement (GenericOrdered a)
  where
  toElement (GenericOrdered x) = genericUnparseElement x

genericParseOrdered ::
  forall a i.
  (GHC.Generic a, GTo a, GenericParseOrdered a (GDatatypeInfoOf a) (GCode a)) =>
  AnnotatedElement i ->
  Parser i a
genericParseOrdered el = gto <$> genericParseOrdered' @a @(GDatatypeInfoOf a) @(GCode a) el

class (MapNamesToXML a) => GenericParseOrdered (a :: Type) (info :: DatatypeInfo) (code :: [[Type]]) where
  genericParseOrdered' :: AnnotatedElement i -> Parser i (SOP I code)

class (MapNamesToXML a) => GenericParseOrdered' (a :: Type) (constructors :: [ConstructorInfo]) (code :: [[Type]]) where
  genericParseOrdered'' :: AnnotatedElement i -> Parser i (NS (NP I) code)

instance
  ( GenericParseOrdered' a constructors codes
  ) =>
  GenericParseOrdered a ('ADT _m _d constructors _s) codes
  where
  genericParseOrdered' el = SOP <$> genericParseOrdered'' @a @constructors @codes el

class (MapNamesToXML a) => GenericParseOrdered'' (a :: Type) (fields :: [FieldInfo]) (code :: [Type]) where
  genericParseOrdered''' :: OrderedM i (NP I code)

instance (MapNamesToXML a) => GenericParseOrdered' a '[] '[] where
  genericParseOrdered'' Element {info} = Left . parserError info $ "No constructor matched."

instance
  ( MapNamesToXML a,
    GenericParseOrdered' a constructors codes,
    GenericParseOrdered'' a fields code
  ) =>
  GenericParseOrdered' a ('Record _c fields ': constructors) (code ': codes)
  where
  genericParseOrdered'' el =
    (Z <$> parseOrderedElement (genericParseOrdered''' @a @fields @code) el)
      <> (S <$> genericParseOrdered'' @a @constructors @codes el)

instance (MapNamesToXML a) => GenericParseOrdered'' a '[] '[] where
  genericParseOrdered''' = pure Nil

instance
  ( MapNamesToXML a,
    GenericParseOrderedField a field (ClassifyFieldLabel field) code,
    GenericParseOrdered'' a fields codes
  ) =>
  GenericParseOrdered'' (a :: Type) (field ': fields) (code ': codes)
  where
  genericParseOrdered''' = do
    x <- genericParseOrderedField @a @field @(ClassifyFieldLabel field) @code
    rest <- genericParseOrdered''' @a @fields @codes
    pure (I x :* rest)

class (MapNamesToXML a) => GenericParseOrderedField (a :: Type) (field :: FieldInfo) (l :: LabelClass) (code :: Type) where
  genericParseOrderedField :: OrderedM i code

instance
  {-# OVERLAPPING #-}
  ( MapNamesToXML a,
    FromElement code,
    KnownSymbol field
  ) =>
  GenericParseOrderedField (a :: Type) ('FieldInfo field) 'ElementClass (Maybe code)
  where
  genericParseOrderedField =
    Ordered.consumeElementOrAbsent (mapNameToElement @a @field)

instance
  ( MapNamesToXML a,
    FromElement code,
    KnownSymbol field
  ) =>
  GenericParseOrderedField (a :: Type) ('FieldInfo field) 'ElementClass code
  where
  genericParseOrderedField =
    Ordered.consumeElement (mapNameToElement @a @field)

instance
  {-# OVERLAPPING #-}
  ( MapNamesToXML a,
    FromElement code,
    KnownSymbol field
  ) =>
  GenericParseOrderedField (a :: Type) ('FieldInfo field) 'ElementClass [code]
  where
  genericParseOrderedField =
    Ordered.consumeElements (mapNameToElement @a @field)

instance
  {-# OVERLAPPING #-}
  ( MapNamesToXML a,
    MkRestricted rs code,
    GenericParseOrderedField a ('FieldInfo field) 'ElementClass code
  ) =>
  GenericParseOrderedField (a :: Type) ('FieldInfo field) 'ElementClass (Restricted rs code)
  where
  genericParseOrderedField = do
    a <- genericParseOrderedField @a @('FieldInfo field) @'ElementClass @code
    case mkRestricted a of
      Right b -> pure b
      Left err -> do
        Element {info} <- get
        throwError $ parserError info err

instance
  {-# OVERLAPPING #-}
  ( MapNamesToXML a,
    FromElement code,
    KnownSymbol field
  ) =>
  GenericParseOrderedField (a :: Type) ('FieldInfo field) 'ElementClass (NonEmpty code)
  where
  genericParseOrderedField = do
    x <- Ordered.consumeElement $ mapNameToElement @a @field
    xs <- Ordered.consumeElements $ mapNameToElement @a @field
    pure $ x :| xs

instance
  {-# OVERLAPPING #-}
  (MapNamesToXML a) =>
  GenericParseOrderedField (a :: Type) ('FieldInfo _info) 'ElementClass Leftovers
  where
  genericParseOrderedField = Ordered.consumeLeftovers

instance
  ( MapNamesToXML a,
    FromContent code,
    KnownSymbol field
  ) =>
  GenericParseOrderedField (a :: Type) ('FieldInfo field) 'AttributeClass code
  where
  genericParseOrderedField = do
    Ordered.consumeAttribute (mapNameToAttribute @a @field)

instance
  {-# OVERLAPPING #-}
  ( MapNamesToXML a,
    FromContent code,
    KnownSymbol field
  ) =>
  GenericParseOrderedField (a :: Type) ('FieldInfo field) 'AttributeClass (Maybe code)
  where
  genericParseOrderedField = do
    Ordered.consumeOptionalAttribute (mapNameToAttribute @a @field)

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

genericParseUnordered :: forall a i. (GHC.Generic a, GTo a, GenericParseUnordered a (GDatatypeInfoOf a) (GCode a)) => AnnotatedElement i -> Parser i a
genericParseUnordered el = gto <$> genericParseUnordered' @a @(GDatatypeInfoOf a) @(GCode a) el

class (MapNamesToXML a) => GenericParseUnordered (a :: Type) (info :: DatatypeInfo) (code :: [[Type]]) where
  genericParseUnordered' :: AnnotatedElement i -> Parser i (SOP I code)

instance
  (GenericParseUnordered' a fields code) =>
  GenericParseUnordered a ('ADT _m _d '[ 'Record _c fields] _s) '[code]
  where
  genericParseUnordered' el = SOP . Z <$> parseUnorderedElement (genericParseUnordered'' @a @fields @code) el

class (MapNamesToXML a) => GenericParseUnordered' (a :: Type) (fields :: [FieldInfo]) (code :: [Type]) where
  genericParseUnordered'' :: UnorderedM i (NP I code)

instance (MapNamesToXML a) => GenericParseUnordered' (a :: Type) '[] '[] where
  genericParseUnordered'' = pure Nil

instance
  ( MapNamesToXML a,
    GenericParseUnorderedField a field (ClassifyFieldLabel field) code,
    GenericParseUnordered' a fields codes
  ) =>
  GenericParseUnordered' (a :: Type) (field ': fields) (code ': codes)
  where
  genericParseUnordered'' = do
    x <- genericParseUnorderedField @a @field @(ClassifyFieldLabel field) @code
    rest <- genericParseUnordered'' @a @fields @codes
    pure (I x :* rest)

class (MapNamesToXML a) => GenericParseUnorderedField (a :: Type) (field :: FieldInfo) (l :: LabelClass) (code :: Type) where
  genericParseUnorderedField :: UnorderedM i code

instance
  {-# OVERLAPPING #-}
  ( MapNamesToXML a,
    FromElement code,
    KnownSymbol field
  ) =>
  GenericParseUnorderedField (a :: Type) ('FieldInfo field) 'ElementClass (Maybe code)
  where
  genericParseUnorderedField =
    Unordered.consumeElementOrAbsent (mapNameToElement @a @field)

instance
  ( MapNamesToXML a,
    FromElement code,
    KnownSymbol field
  ) =>
  GenericParseUnorderedField (a :: Type) ('FieldInfo field) 'ElementClass code
  where
  genericParseUnorderedField =
    Unordered.consumeElement (mapNameToElement @a @field)

instance
  {-# OVERLAPPING #-}
  ( MapNamesToXML a,
    FromElement code,
    KnownSymbol field
  ) =>
  GenericParseUnorderedField (a :: Type) ('FieldInfo field) 'ElementClass [code]
  where
  genericParseUnorderedField =
    Unordered.consumeElements (mapNameToElement @a @field)

instance
  {-# OVERLAPPING #-}
  ( MapNamesToXML a,
    FromElement code,
    KnownSymbol field
  ) =>
  GenericParseUnorderedField (a :: Type) ('FieldInfo field) 'ElementClass (NonEmpty code)
  where
  genericParseUnorderedField = do
    x <- Unordered.consumeElement $ mapNameToElement @a @field
    xs <- Unordered.consumeElements $ mapNameToElement @a @field
    pure $ x :| xs

instance
  {-# OVERLAPPING #-}
  (MapNamesToXML a) =>
  GenericParseUnorderedField (a :: Type) _info _label Leftovers
  where
  genericParseUnorderedField = Unordered.consumeLeftovers

instance
  ( MapNamesToXML a,
    FromContent code,
    KnownSymbol field
  ) =>
  GenericParseUnorderedField (a :: Type) ('FieldInfo field) 'AttributeClass code
  where
  genericParseUnorderedField = do
    Unordered.consumeAttribute (mapNameToAttribute @a @field)

instance
  {-# OVERLAPPING #-}
  ( MapNamesToXML a,
    FromContent code,
    KnownSymbol field
  ) =>
  GenericParseUnorderedField (a :: Type) ('FieldInfo field) 'AttributeClass (Maybe code)
  where
  genericParseUnorderedField = do
    Unordered.consumeOptionalAttribute (mapNameToAttribute @a @field)

{-
instance
  ( MapNamesToXML a,
    FromChoiceElement code
  ) =>
  GenericParseUnorderedField (a :: Type) ('FieldInfo field) 'ChoiceClass code
  where
  genericParseUnorderedField = Unordered.consumeChoiceElement
-}

-- could unify with GenericOrdered?
newtype GenericChoice a = GenericChoice a

instance
  (GHC.Generic a, GTo a, GenericParseChoice a (GDatatypeInfoOf a) (GCode a)) =>
  FromElement (GenericChoice a)
  where
  fromElement el = GenericChoice <$> genericParseChoice el

{-
instance
  (GHC.Generic a, GFrom a, GenericUnparseChoice a (GDatatypeInfoOf a) (GCode a)) =>
  ToElement (GenericChoice a)
  where
  toElement (GenericChoice x) = genericUnparseChoice x
-}
genericParseChoice ::
  forall a i.
  (GHC.Generic a, GTo a, GenericParseChoice a (GDatatypeInfoOf a) (GCode a)) =>
  AnnotatedElement i ->
  Parser i a
genericParseChoice el = gto <$> genericParseChoice' @a @(GDatatypeInfoOf a) @(GCode a) el

class (MapNamesToXML a) => GenericParseChoice (a :: Type) (info :: DatatypeInfo) (code :: [[Type]]) where
  genericParseChoice' :: AnnotatedElement i -> Parser i (SOP I code)

instance
  (GenericParseChoice' a constructors code) =>
  GenericParseChoice a ('ADT _m _d constructors _s) code
  where
  genericParseChoice' el = SOP <$> parseOrderedElement (genericParseChoice'' @a @constructors @code) el

class (MapNamesToXML a) => GenericParseChoice' (a :: Type) (constructors :: [ConstructorInfo]) (code :: [[Type]]) where
  genericParseChoice'' :: OrderedM i (NS (NP I) code)

instance (MapNamesToXML a) => GenericParseChoice' (a :: Type) '[] '[] where
  genericParseChoice'' = do
    Element {info} <- get
    throwError $ parserError info "All of the possible choices failed to parse."

instance
  ( MapNamesToXML a,
    FromElement code,
    GenericParseChoice' a constructors codes,
    KnownSymbol label
  ) =>
  GenericParseChoice' (a :: Type) ('Record _n '[ 'FieldInfo label] ': constructors) ('[code] ': codes)
  where
  genericParseChoice'' = do
    mx <- consumeElementOrAbsent $ mapNameToElement @a @label
    case mx of
      Just x -> pure $ Z (I x :* Nil)
      Nothing -> S <$> genericParseChoice'' @a @constructors @codes

instance
  {-# OVERLAPPING #-}
  ( MapNamesToXML a,
    FromElement code,
    GenericParseChoice' a constructors codes,
    KnownSymbol label
  ) =>
  GenericParseChoice' (a :: Type) ('Record _n '[ 'FieldInfo label] ': constructors) ('[NonEmpty code] ': codes)
  where
  genericParseChoice'' = do
    mx <- consumeElementOrAbsent $ mapNameToElement @a @label
    case mx of
      Just x -> do
        xs <- consumeElements $ mapNameToElement @a @label
        pure $ Z (I (x :| xs) :* Nil)
      Nothing -> S <$> genericParseChoice'' @a @constructors @codes

genericUnparseElement ::
  forall a.
  (GHC.Generic a, GFrom a, GenericUnparseElement a (GDatatypeInfoOf a) (GCode a)) =>
  a ->
  Element
genericUnparseElement x = genericUnparseElement' @a @(GDatatypeInfoOf a) @(GCode a) $ gfrom x

class (MapNamesToXML a) => GenericUnparseElement (a :: Type) (info :: DatatypeInfo) (code :: [[Type]]) where
  genericUnparseElement' :: SOP I code -> Element

instance
  (GenericUnparseElement' a constructors code) =>
  GenericUnparseElement a ('ADT _m _d constructors _s) code
  where
  genericUnparseElement' (SOP x) = genericUnparseElement'' @a @constructors @code x

class (MapNamesToXML a) => GenericUnparseElement' (a :: Type) (constructors :: [ConstructorInfo]) (code :: [[Type]]) where
  genericUnparseElement'' :: NS (NP I) code -> Element

instance (MapNamesToXML a) => GenericUnparseElement' a '[] '[] where
  genericUnparseElement'' x = case x of {}

instance
  ( MapNamesToXML a,
    GenericUnparseElement' a constructors codes,
    GenericUnparseElement'' a fields code
  ) =>
  GenericUnparseElement' a ('Record _c fields ': constructors) (code ': codes)
  where
  genericUnparseElement'' x = case x of
    Z y -> genericUnparseElement''' @a @fields @code y
    S y -> genericUnparseElement'' @a @constructors @codes y

class (MapNamesToXML a) => GenericUnparseElement'' (a :: Type) (fields :: [FieldInfo]) (code :: [Type]) where
  genericUnparseElement''' :: NP I code -> Element

instance (MapNamesToXML a) => GenericUnparseElement'' a '[] '[] where
  genericUnparseElement''' Nil = mempty

instance
  ( MapNamesToXML a,
    GenericUnparseField a field (ClassifyFieldLabel field) code,
    GenericUnparseElement'' a fields codes
  ) =>
  GenericUnparseElement'' a (field ': fields) (code ': codes)
  where
  genericUnparseElement''' (I x :* xs) =
    genericUnparseField @a @field @(ClassifyFieldLabel field) @code x
      <> genericUnparseElement''' @a @fields @codes xs

class
  (MapNamesToXML a) =>
  GenericUnparseField (a :: Type) (field :: FieldInfo) (l :: LabelClass) (code :: Type)
  where
  genericUnparseField :: code -> Element

instance
  {-# OVERLAPPING #-}
  (MapNamesToXML a, ToContent code, KnownSymbol field) =>
  GenericUnparseField a ('FieldInfo field) 'AttributeClass (Maybe code)
  where
  genericUnparseField Nothing = mempty
  genericUnparseField (Just code) =
    mempty
      { attributes = Map.singleton (mapNameToAttribute @a @field) (toContent code, ())
      }

instance
  (MapNamesToXML a, ToContent code, KnownSymbol field) =>
  GenericUnparseField a ('FieldInfo field) 'AttributeClass code
  where
  genericUnparseField code =
    mempty
      { attributes = Map.singleton (mapNameToAttribute @a @field) (toContent code, ())
      }

instance
  (MapNamesToXML a, ToElement code, KnownSymbol field) =>
  GenericUnparseField a ('FieldInfo field) 'ElementClass code
  where
  genericUnparseField code =
    mempty
      { children = [NodeElement (mapNameToElement @a @field) (toElement code) ()]
      }

instance
  {-# OVERLAPPING #-}
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
  {-# OVERLAPPING #-}
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
  {-# OVERLAPPING #-}
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

mapNameToElement :: forall a label. (MapNamesToXML a, KnownSymbol label) => Name
mapNameToElement = mapNamesToElements @a . stripElementPrefix . symbolVal $ Proxy @label

mapNameToAttribute :: forall a label. (MapNamesToXML a, KnownSymbol label) => Name
mapNameToAttribute = mapNamesToAttributes @a . stripAttributePrefix . symbolVal $ Proxy @label

mapNameToEnum :: forall a label. (MapNamesToXML a, KnownSymbol label) => String
mapNameToEnum = mapNamesToEnum @a . symbolVal $ Proxy @label

{-
genericUnparseChoice ::
  forall a.
  (GHC.Generic a, GFrom a, GenericUnparseChoice a (GDatatypeInfoOf a) (GCode a)) =>
  a ->
  Element
genericUnparseChoice x =
  let SOP code = gfrom x
   in genericUnparseChoice' @a @(GDatatypeInfoOf a) @(GCode a) code

class (MapNamesToXML a) => GenericUnparseChoice (a :: Type) (info :: DatatypeInfo) (code :: [[Type]]) where
  genericUnparseChoice' :: NS (NP I) code -> Element

instance
  (GenericUnparseChoice' a constructors code) =>
  GenericUnparseChoice a ('ADT _m _d constructors _s) code
  where
  genericUnparseChoice' = genericUnparseChoice'' @a @constructors @code

class
  (MapNamesToXML a) =>
  GenericUnparseChoice' (a :: Type) (constructors :: [ConstructorInfo]) (code :: [[Type]])
  where
  genericUnparseChoice'' :: NS (NP I) code -> Element

instance (MapNamesToXML a) => GenericUnparseChoice' (a :: Type) '[] '[] where
  genericUnparseChoice'' x = case x of {}

instance
  ( MapNamesToXML a,
    ToElement code,
    GenericUnparseChoice' a constructors codes,
    KnownSymbol label
  ) =>
  GenericUnparseChoice' (a :: Type) ('Record _n '[ 'FieldInfo label] ': constructors) ('[code] ': codes)
  where
  genericUnparseChoice'' (Z (I x :* Nil)) = (mapNameToElement @a @label, toElement x)
  genericUnparseChoice'' (S x) = genericUnparseChoice'' @a @constructors @codes x
-}
newtype GenericEnum a = GenericEnum a

instance
  (GHC.Generic a, GTo a, GenericParseEnum a (GDatatypeInfoOf a) (GCode a)) =>
  FromContent (GenericEnum a)
  where
  fromContent t i = GenericEnum <$> genericParseEnum t i

deriving via
  ContentElement (GenericEnum a)
  instance
    (FromContent (GenericEnum a)) =>
    FromElement (GenericEnum a)

instance
  (GHC.Generic a, GFrom a, GenericUnparseEnum a (GDatatypeInfoOf a) (GCode a)) =>
  ToContent (GenericEnum a)
  where
  toContent (GenericEnum x) = genericUnparseEnum x

genericParseEnum :: forall a i. (GHC.Generic a, GTo a, GenericParseEnum a (GDatatypeInfoOf a) (GCode a)) => Text -> i -> Parser i a
genericParseEnum t i = gto <$> genericParseEnum' @a @(GDatatypeInfoOf a) @(GCode a) t i

class
  (MapNamesToXML a) =>
  GenericParseEnum (a :: Type) (info :: DatatypeInfo) (code :: [[Type]])
  where
  genericParseEnum' :: Text -> i -> Parser i (SOP I code)

instance
  (GenericParseEnum' a constructors code) =>
  GenericParseEnum a ('ADT _m _d constructors _s) code
  where
  genericParseEnum' t i = SOP <$> genericParseEnum'' @a @constructors @code t i

class
  (MapNamesToXML a) =>
  GenericParseEnum' (a :: Type) (constructors :: [ConstructorInfo]) (code :: [[Type]])
  where
  genericParseEnum'' :: Text -> i -> Parser i (NS (NP I) code)

instance
  ( MapNamesToXML a,
    KnownSymbol name,
    GenericParseEnum' a constructors codes
  ) =>
  GenericParseEnum' a ('Constructor name ': constructors) ('[] ': codes)
  where
  genericParseEnum'' t i =
    if t == T.pack (mapNameToEnum @a @name)
      then pure $ Z Nil
      else case genericParseEnum'' @a @constructors @codes t i of
        Right x -> pure $ S x
        Left err -> Left err {message = message err <> " | enum value " <> mapNameToEnum @a @name}

instance
  ( MapNamesToXML a,
    FromContent code,
    GenericParseEnum' a constructors codes
  ) =>
  GenericParseEnum' a (_c ': constructors) ('[code] ': codes)
  where
  genericParseEnum'' t i =
    case fromContent t i of
      Right a -> pure $ Z (I a :* Nil)
      Left err -> case genericParseEnum'' @a @constructors @codes t i of
        Right a -> pure $ S a
        Left err2 -> Left err2 {message = message err <> " | " <> message err2}

instance (MapNamesToXML a) => GenericParseEnum' a '[] '[] where
  genericParseEnum'' _ i = Left $ parserError i "Could not parse enum"

genericUnparseEnum ::
  forall a.
  (GHC.Generic a, GFrom a, GenericUnparseEnum a (GDatatypeInfoOf a) (GCode a)) =>
  a ->
  Text
genericUnparseEnum a = genericUnparseEnum' @a @(GDatatypeInfoOf a) @(GCode a) $ gfrom a

class
  (MapNamesToXML a) =>
  GenericUnparseEnum (a :: Type) (info :: DatatypeInfo) (code :: [[Type]])
  where
  genericUnparseEnum' :: SOP I code -> Text

instance
  (GenericUnparseEnum' a constructors code) =>
  GenericUnparseEnum a ('ADT _m _d constructors _s) code
  where
  genericUnparseEnum' (SOP x) = genericUnparseEnum'' @a @constructors @code x

class
  (MapNamesToXML a) =>
  GenericUnparseEnum' (a :: Type) (constructors :: [ConstructorInfo]) (code :: [[Type]])
  where
  genericUnparseEnum'' :: NS (NP I) code -> Text

instance
  ( MapNamesToXML a,
    KnownSymbol name,
    GenericUnparseEnum' a constructors codes
  ) =>
  GenericUnparseEnum' a ('Constructor name ': constructors) ('[] ': codes)
  where
  genericUnparseEnum'' (Z _) = T.pack $ mapNameToEnum @a @name
  genericUnparseEnum'' (S x) = genericUnparseEnum'' @a @constructors @codes x

instance (MapNamesToXML a) => GenericUnparseEnum' a '[] '[] where
  genericUnparseEnum'' x = case x of {}

instance
  ( MapNamesToXML a,
    GenericUnparseEnum' a constructors codes,
    ToContent code
  ) =>
  GenericUnparseEnum' a (_c ': constructors) ('[code] ': codes)
  where
  genericUnparseEnum'' (Z (I x :* Nil)) = toContent x
  genericUnparseEnum'' (S x) = genericUnparseEnum'' @a @constructors @codes x

newtype GenericDocument a = GenericDocument a

instance
  (GHC.Generic a, GTo a, GenericParseDocument a (GDatatypeInfoOf a) (GCode a)) =>
  FromDocument (GenericDocument a)
  where
  fromDocument el = GenericDocument <$> genericParseDocument el

instance
  (GHC.Generic a, GFrom a, GenericUnparseDocument a (GDatatypeInfoOf a) (GCode a)) =>
  ToDocument (GenericDocument a)
  where
  toDocument (GenericDocument x) = genericUnparseDocument x

genericParseDocument ::
  forall a i.
  (GHC.Generic a, GTo a, GenericParseDocument a (GDatatypeInfoOf a) (GCode a)) =>
  AnnotatedDocument i ->
  Parser i a
genericParseDocument doc = gto <$> genericParseDocument' @a @(GDatatypeInfoOf a) @(GCode a) doc

class (MapNamesToXML a) => GenericParseDocument (a :: Type) (info :: DatatypeInfo) (code :: [[Type]]) where
  genericParseDocument' :: AnnotatedDocument i -> Parser i (SOP I code)

instance
  (MapNamesToXML a, FromElement root, KnownSymbol field) =>
  GenericParseDocument a ('ADT _m _d '[ 'Record _c '[ 'FieldInfo field]] _s) '[ '[root]]
  where
  genericParseDocument' doc =
    (\r -> SOP . Z $ I r :* Nil) <$> fromRootElement (mapNameToElement @a @field) doc

genericUnparseDocument ::
  forall a.
  (GHC.Generic a, GFrom a, GenericUnparseDocument a (GDatatypeInfoOf a) (GCode a)) =>
  a ->
  Document
genericUnparseDocument x = genericUnparseDocument' @a @(GDatatypeInfoOf a) @(GCode a) $ gfrom x

class (MapNamesToXML a) => GenericUnparseDocument (a :: Type) (info :: DatatypeInfo) (code :: [[Type]]) where
  genericUnparseDocument' :: SOP I code -> Document

instance
  (MapNamesToXML a, ToElement root, KnownSymbol field) =>
  GenericUnparseDocument a ('ADT _m _d '[ 'Record _c '[ 'FieldInfo field]] _s) '[ '[root]]
  where
  genericUnparseDocument' (SOP x) =
    case x of
      Z (I y :* Nil) -> toRootElement (mapNameToElement @a @field) y
      S y -> case y of {}

newtype GenericContent a = GenericContent a

instance
  (GHC.Generic a, GTo a, GenericParseContent a (GDatatypeInfoOf a) (GCode a)) =>
  FromElement (GenericContent a)
  where
  fromElement el = GenericContent <$> genericParseContent el

genericParseContent ::
  forall a i.
  (GHC.Generic a, GTo a, GenericParseContent a (GDatatypeInfoOf a) (GCode a)) =>
  AnnotatedElement i ->
  Parser i a
genericParseContent el = gto <$> genericParseContent' @a @(GDatatypeInfoOf a) @(GCode a) el

class (MapNamesToXML a) => GenericParseContent (a :: Type) (info :: DatatypeInfo) (code :: [[Type]]) where
  genericParseContent' :: AnnotatedElement i -> Parser i (SOP I code)

instance
  (GenericParseContent' a fields code) =>
  GenericParseContent a ('ADT _m _d '[ 'Record _c fields] _s) '[code]
  where
  genericParseContent' el = SOP . Z <$> parseOrderedElement (genericParseContent'' @a @fields @code) el

class (MapNamesToXML a) => GenericParseContent' (a :: Type) (fields :: [FieldInfo]) (code :: [Type]) where
  genericParseContent'' :: OrderedM i (NP I code)

instance (MapNamesToXML a) => GenericParseContent' (a :: Type) '[] '[] where
  genericParseContent'' = pure Nil

instance
  ( MapNamesToXML a,
    GenericParseContentField a field (ClassifyContentLabel field) code,
    GenericParseContent' a fields codes
  ) =>
  GenericParseContent' (a :: Type) (field ': fields) (code ': codes)
  where
  genericParseContent'' = do
    x <- genericParseContentField @a @field @(ClassifyContentLabel field) @code
    rest <- genericParseContent'' @a @fields @codes
    pure (I x :* rest)

class
  (MapNamesToXML a) =>
  GenericParseContentField (a :: Type) (field :: FieldInfo) (l :: LabelClass) (code :: Type)
  where
  genericParseContentField :: OrderedM i code -- ContentM?

instance
  ( MapNamesToXML a,
    GenericParseOrderedField a fieldinfo 'AttributeClass code
  ) =>
  GenericParseContentField (a :: Type) fieldinfo 'AttributeClass code
  where
  genericParseContentField = genericParseOrderedField @a @fieldinfo @'AttributeClass @code

instance
  ( MapNamesToXML a,
    FromContent code
  ) =>
  GenericParseContentField (a :: Type) fieldinfo 'ContentClass code
  where
  genericParseContentField = do
    el@Element {children, info} <- get
    case children of
      NodeContent c i : rest -> do
        put el {children = rest}
        either throwError pure $ fromContent c i
      NodeElement _ _ i : _ ->
        throwError . parserError i $
          "Unexpected child node when parsing content element!"
      [] -> throwError $ parserError info "Missing content (reached end of element)."
