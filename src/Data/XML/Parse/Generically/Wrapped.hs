{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.XML.Parse.Generically.Wrapped where

{- ( MapNamesToXML (..),
   GenericParseOrdered (..),
   genericParseOrdered,
   GenericParseUnordered (..),
   genericParseUnordered,
   MaybeAbsent (..),
   GenericParseDocument (..),
   GenericOrdered (..),
   GenericDocument (..),
   GenericUnordered (..),
   MapNamesId (..),
   Attribute (..),
   Choice (..),
 )-}

import Data.Bifunctor
import Data.Kind
import Data.List.NonEmpty hiding (map)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Data.String (IsString (..))
import Data.XML
import Data.XML.Parse.Ordered (OrderedM, parseOrderedElement)
import qualified Data.XML.Parse.Ordered as Ordered
import Data.XML.Parse.Unordered (UnorderedM, parseUnorderedElement)
import qualified Data.XML.Parse.Unordered as Unordered
import qualified GHC.Generics as GHC
import GHC.TypeLits (KnownSymbol, symbolVal)
import Generics.SOP (NP (..), NS (..), SOP (..))
import Generics.SOP.BasicFunctors
import Generics.SOP.GGP
import Generics.SOP.Type.Metadata

{-
newtype Attribute a = Attribute a

newtype Choice a = Choice a

newtype MaybeAbsent a = MaybeAbsent (Maybe a)
  deriving stock (Show, Eq, Ord, GHC.Generic)

class MapNamesToXML a where
  mapNamesToXML :: String -> Name

newtype GenericOrdered a = GenericOrdered a

instance
  (GHC.Generic a, GTo a, GenericParseOrdered a (GDatatypeInfoOf a) (GCode a)) =>
  FromElement (GenericOrdered a)
  where
  fromElement el = GenericOrdered <$> genericParseOrdered el

genericParseOrdered :: forall a i. (GHC.Generic a, GTo a, GenericParseOrdered a (GDatatypeInfoOf a) (GCode a)) => AnnotatedElement i -> Parser i a
genericParseOrdered el = gto <$> genericParseOrdered' @a @(GDatatypeInfoOf a) @(GCode a) el

class MapNamesToXML a => GenericParseOrdered (a :: Type) (info :: DatatypeInfo) (code :: [[Type]]) where
  genericParseOrdered' :: AnnotatedElement i -> Parser i (SOP I code)

instance
  GenericParseOrdered' a fields code =>
  GenericParseOrdered a ('ADT _m _d '[ 'Record _c fields] _s) '[code]
  where
  genericParseOrdered' el = SOP . Z <$> parseOrderedElement (genericParseOrdered'' @a @fields @code) el

class MapNamesToXML a => GenericParseOrdered' (a :: Type) (fields :: [FieldInfo]) (code :: [Type]) where
  genericParseOrdered'' :: OrderedM i (NP I code)

instance MapNamesToXML a => GenericParseOrdered' (a :: Type) '[] '[] where
  genericParseOrdered'' = pure Nil

instance
  {-# OVERLAPPABLE #-}
  ( MapNamesToXML a,
    FromElement code,
    GenericParseOrdered' a fields codes,
    KnownSymbol field
  ) =>
  GenericParseOrdered' (a :: Type) ('FieldInfo field ': fields) (code ': codes)
  where
  genericParseOrdered'' = do
    x <- Ordered.consumeElement (mapNamesToXML @a . symbolVal $ Proxy @field)
    rest <- genericParseOrdered'' @a @fields @codes
    pure (I x :* rest)

instance
  ( MapNamesToXML a,
    FromElement code,
    GenericParseOrdered' a fields codes,
    KnownSymbol field
  ) =>
  GenericParseOrdered' (a :: Type) ('FieldInfo field ': fields) (MaybeAbsent code ': codes)
  where
  genericParseOrdered'' = do
    x <- Ordered.consumeElementOrAbsent (mapNamesToXML @a . symbolVal $ Proxy @field)
    rest <- genericParseOrdered'' @a @fields @codes
    pure (I (MaybeAbsent x) :* rest)

instance
  ( MapNamesToXML a,
    FromChoiceElement code,
    GenericParseOrdered' a fields codes
  ) =>
  GenericParseOrdered' (a :: Type) ('FieldInfo _field ': fields) (Choice code ': codes)
  where
  genericParseOrdered'' = do
    x <- Ordered.consumeChoiceElement
    rest <- genericParseOrdered'' @a @fields @codes
    pure (I (Choice x) :* rest)

instance
  ( MapNamesToXML a,
    FromContent code,
    GenericParseOrdered' a fields codes,
    KnownSymbol field
  ) =>
  GenericParseOrdered' (a :: Type) ('FieldInfo field ': fields) (Attribute code ': codes)
  where
  genericParseOrdered'' = do
    x <- Ordered.consumeAttribute (mapNamesToXML @a . symbolVal $ Proxy @field)
    rest <- genericParseOrdered'' @a @fields @codes
    pure (I (Attribute x) :* rest)

instance
  {-# OVERLAPPING #-}
  ( MapNamesToXML a,
    FromContent code,
    GenericParseOrdered' a fields codes,
    KnownSymbol field
  ) =>
  GenericParseOrdered' (a :: Type) ('FieldInfo field ': fields) (Attribute (Maybe code) ': codes)
  where
  genericParseOrdered'' = do
    x <- Ordered.consumeOptionalAttribute (mapNamesToXML @a . symbolVal $ Proxy @field)
    rest <- genericParseOrdered'' @a @fields @codes
    pure (I (Attribute x) :* rest)

instance
  ( MapNamesToXML a,
    FromElement code,
    GenericParseOrdered' a fields codes,
    KnownSymbol field
  ) =>
  GenericParseOrdered' (a :: Type) ('FieldInfo field ': fields) ([code] ': codes)
  where
  genericParseOrdered'' = do
    xs <- Ordered.consumeElements (mapNamesToXML @a . symbolVal $ Proxy @field)
    rest <- genericParseOrdered'' @a @fields @codes
    pure (I xs :* rest)

instance
  ( MapNamesToXML a,
    FromElement code,
    GenericParseOrdered' a fields codes,
    KnownSymbol field
  ) =>
  GenericParseOrdered' (a :: Type) ('FieldInfo field ': fields) (NonEmpty code ': codes)
  where
  genericParseOrdered'' = do
    let n = mapNamesToXML @a . symbolVal $ Proxy @field
    x <- Ordered.consumeElement n
    xs <- Ordered.consumeElements n
    rest <- genericParseOrdered'' @a @fields @codes
    pure (I (x :| xs) :* rest)

-- It rarely makes sense to have a 'Leftovers' fields _not_ at the end of the
-- record. The remaining fields must succeed on the empty element.
instance
  (GenericParseOrdered' a fields codes) =>
  GenericParseOrdered' (a :: Type) ('FieldInfo _field ': fields) (Leftovers ': codes)
  where
  genericParseOrdered'' = do
    leftovers <- Ordered.consumeLeftovers
    rest <- genericParseOrdered'' @a @fields @codes
    pure (I leftovers :* rest)

newtype GenericUnordered a = GenericUnordered a

instance
  (GHC.Generic a, GTo a, GenericParseUnordered a (GDatatypeInfoOf a) (GCode a)) =>
  FromElement (GenericUnordered a)
  where
  fromElement el = GenericUnordered <$> genericParseUnordered el

genericParseUnordered :: forall a i. (GHC.Generic a, GTo a, GenericParseUnordered a (GDatatypeInfoOf a) (GCode a)) => AnnotatedElement i -> Parser i a
genericParseUnordered el = gto <$> genericParseUnordered' @a @(GDatatypeInfoOf a) @(GCode a) el

class MapNamesToXML a => GenericParseUnordered (a :: Type) (info :: DatatypeInfo) (code :: [[Type]]) where
  genericParseUnordered' :: AnnotatedElement i -> Parser i (SOP I code)

instance
  GenericParseUnordered' a fields code =>
  GenericParseUnordered a ('ADT _m _d '[ 'Record _c fields] _s) '[code]
  where
  genericParseUnordered' el = SOP . Z <$> parseUnorderedElement (genericParseUnordered'' @a @fields @code) el

class MapNamesToXML a => GenericParseUnordered' (a :: Type) (fields :: [FieldInfo]) (code :: [Type]) where
  genericParseUnordered'' :: UnorderedM i (NP I code)

instance MapNamesToXML a => GenericParseUnordered' (a :: Type) '[] '[] where
  genericParseUnordered'' = pure Nil

instance
  {-# OVERLAPPABLE #-}
  ( MapNamesToXML a,
    FromElement code,
    GenericParseUnordered' a fields codes,
    KnownSymbol field
  ) =>
  GenericParseUnordered' (a :: Type) ('FieldInfo field ': fields) (code ': codes)
  where
  genericParseUnordered'' = do
    x <- Unordered.consumeElement (mapNamesToXML @a . symbolVal $ Proxy @field)
    rest <- genericParseUnordered'' @a @fields @codes
    pure (I x :* rest)

instance
  ( MapNamesToXML a,
    FromElement code,
    GenericParseUnordered' a fields codes,
    KnownSymbol field
  ) =>
  GenericParseUnordered' (a :: Type) ('FieldInfo field ': fields) (MaybeAbsent code ': codes)
  where
  genericParseUnordered'' = do
    x <- Unordered.consumeElementOrAbsent (mapNamesToXML @a . symbolVal $ Proxy @field)
    rest <- genericParseUnordered'' @a @fields @codes
    pure (I (MaybeAbsent x) :* rest)

instance
  ( MapNamesToXML a,
    FromElement code,
    GenericParseUnordered' a fields codes,
    KnownSymbol field
  ) =>
  GenericParseUnordered' (a :: Type) ('FieldInfo field ': fields) ([code] ': codes)
  where
  genericParseUnordered'' = do
    xs <- Unordered.consumeElements (mapNamesToXML @a . symbolVal $ Proxy @field)
    rest <- genericParseUnordered'' @a @fields @codes
    pure (I xs :* rest)

instance
  ( MapNamesToXML a,
    FromElement code,
    GenericParseUnordered' a fields codes,
    KnownSymbol field
  ) =>
  GenericParseUnordered' (a :: Type) ('FieldInfo field ': fields) (NonEmpty code ': codes)
  where
  genericParseUnordered'' = do
    let n = mapNamesToXML @a . symbolVal $ Proxy @field
    x <- Unordered.consumeElement n
    xs <- Unordered.consumeElements n
    rest <- genericParseUnordered'' @a @fields @codes
    pure (I (x :| xs) :* rest)

-- It rarely makes sense to have a 'Leftovers' fields _not_ at the end of the
-- record. The remaining fields must succeed on the empty element.
instance
  (GenericParseUnordered' a fields codes) =>
  GenericParseUnordered' (a :: Type) ('FieldInfo _field ': fields) (Leftovers ': codes)
  where
  genericParseUnordered'' = do
    leftovers <- Unordered.consumeLeftovers
    rest <- genericParseUnordered'' @a @fields @codes
    pure (I leftovers :* rest)

instance
  ( MapNamesToXML a,
    FromChoiceElement code,
    GenericParseUnordered' a fields codes
  ) =>
  GenericParseUnordered' (a :: Type) ('FieldInfo _field ': fields) (Choice code ': codes)
  where
  genericParseUnordered'' = do
    x <- Unordered.consumeChoiceElement
    rest <- genericParseUnordered'' @a @fields @codes
    pure (I (Choice x) :* rest)

instance
  ( MapNamesToXML a,
    FromContent code,
    GenericParseUnordered' a fields codes,
    KnownSymbol field
  ) =>
  GenericParseUnordered' (a :: Type) ('FieldInfo field ': fields) (Attribute code ': codes)
  where
  genericParseUnordered'' = do
    x <- Unordered.consumeAttribute (mapNamesToXML @a . symbolVal $ Proxy @field)
    rest <- genericParseUnordered'' @a @fields @codes
    pure (I (Attribute x) :* rest)

instance
  {-# OVERLAPPING #-}
  ( MapNamesToXML a,
    FromContent code,
    GenericParseUnordered' a fields codes,
    KnownSymbol field
  ) =>
  GenericParseUnordered' (a :: Type) ('FieldInfo field ': fields) (Attribute (Maybe code) ': codes)
  where
  genericParseUnordered'' = do
    x <- Unordered.consumeOptionalAttribute (mapNamesToXML @a . symbolVal $ Proxy @field)
    rest <- genericParseUnordered'' @a @fields @codes
    pure (I (Attribute x) :* rest)

newtype GenericDocument a = GenericDocument a

instance
  (GHC.Generic a, GTo a, GenericParseDocument a (GDatatypeInfoOf a) (GCode a)) =>
  FromDocument (GenericDocument a)
  where
  fromDocument doc = GenericDocument <$> genericParseDocument doc

genericParseDocument :: forall a i. (GHC.Generic a, GTo a, GenericParseDocument a (GDatatypeInfoOf a) (GCode a)) => AnnotatedDocument i -> Parser i a
genericParseDocument doc = gto <$> genericParseDocument' @a @(GDatatypeInfoOf a) @(GCode a) doc

class MapNamesToXML a => GenericParseDocument (a :: Type) (info :: DatatypeInfo) (code :: [[Type]]) where
  genericParseDocument' :: AnnotatedDocument i -> Parser i (SOP I code)

instance
  (MapNamesToXML a, FromElement code, KnownSymbol field) =>
  GenericParseDocument a ('ADT _m _d '[ 'Record _c '[ 'FieldInfo field]] _s) '[ '[code]]
  where
  genericParseDocument' doc = do
    r <- fromRootElement (mapNamesToXML @a . symbolVal $ Proxy @field) doc
    pure . SOP . Z $ I r :* Nil

newtype MapNamesId a = MapNamesId a

instance MapNamesToXML (MapNamesId a) where
  mapNamesToXML = fromString

newtype GenericAttributes a = GenericAttributes a

instance
  (GHC.Generic a, GTo a, GenericParseAttributes a (GDatatypeInfoOf a) (GCode a)) =>
  FromElement (GenericAttributes a)
  where
  fromElement el = GenericAttributes <$> genericParseAttributes el

genericParseAttributes :: forall a i. (GHC.Generic a, GTo a, GenericParseAttributes a (GDatatypeInfoOf a) (GCode a)) => AnnotatedElement i -> Parser i a
genericParseAttributes el = gto <$> genericParseAttributes' @a @(GDatatypeInfoOf a) @(GCode a) el

class MapNamesToXML a => GenericParseAttributes (a :: Type) (info :: DatatypeInfo) (code :: [[Type]]) where
  genericParseAttributes' :: AnnotatedElement i -> Parser i (SOP I code)

instance
  GenericParseAttributes' a fields code =>
  GenericParseAttributes a ('ADT _m _d '[ 'Record _c fields] _s) '[code]
  where
  genericParseAttributes' el = SOP . Z <$> parseOrderedElement (genericParseAttributes'' @a @fields @code) el

class MapNamesToXML a => GenericParseAttributes' (a :: Type) (fields :: [FieldInfo]) (code :: [Type]) where
  genericParseAttributes'' :: OrderedM i (NP I code)

instance MapNamesToXML a => GenericParseAttributes' (a :: Type) '[] '[] where
  genericParseAttributes'' = pure Nil

instance
  {-# OVERLAPPABLE #-}
  ( MapNamesToXML a,
    FromContent code,
    GenericParseAttributes' a fields codes,
    KnownSymbol field
  ) =>
  GenericParseAttributes' (a :: Type) ('FieldInfo field ': fields) (code ': codes)
  where
  genericParseAttributes'' = do
    x <- Ordered.consumeAttribute (mapNamesToXML @a . symbolVal $ Proxy @field)
    rest <- genericParseAttributes'' @a @fields @codes
    pure (I x :* rest)

instance
  ( MapNamesToXML a,
    FromContent code,
    GenericParseAttributes' a fields codes,
    KnownSymbol field
  ) =>
  GenericParseAttributes' (a :: Type) ('FieldInfo field ': fields) (Maybe code ': codes)
  where
  genericParseAttributes'' = do
    x <- Ordered.consumeOptionalAttribute (mapNamesToXML @a . symbolVal $ Proxy @field)
    rest <- genericParseAttributes'' @a @fields @codes
    pure (I x :* rest)

instance
  {-# OVERLAPPING #-}
  ( MapNamesToXML a,
    FromContent code,
    GenericParseAttributes' a fields codes,
    KnownSymbol field
  ) =>
  GenericParseAttributes' (a :: Type) ('FieldInfo field ': fields) (Attribute (Maybe code) ': codes)
  where
  genericParseAttributes'' = do
    x <- Ordered.consumeOptionalAttribute (mapNamesToXML @a . symbolVal $ Proxy @field)
    rest <- genericParseAttributes'' @a @fields @codes
    pure (I (Attribute x) :* rest)

-- It rarely makes sense to have a 'Leftovers' fields _not_ at the end of the
-- record. The remaining fields must succeed on the empty element.
instance
  (GenericParseAttributes' a fields codes) =>
  GenericParseAttributes' (a :: Type) ('FieldInfo _field ': fields) (Leftovers ': codes)
  where
  genericParseAttributes'' = do
    leftovers <- Ordered.consumeLeftovers
    rest <- genericParseAttributes'' @a @fields @codes
    pure (I leftovers :* rest)

newtype GenericChoice a = GenericChoice a

instance
  (GHC.Generic a, GTo a, GenericParseChoice a (GDatatypeInfoOf a) (GCode a)) =>
  FromChoiceElement (GenericChoice a)
  where
  fromChoiceElement = Map.map (\p el -> GenericChoice <$> p el) genericParseChoice

genericParseChoice :: forall a i. (GHC.Generic a, GTo a, GenericParseChoice a (GDatatypeInfoOf a) (GCode a)) => Map Name (AnnotatedElement i -> Parser i a)
genericParseChoice = Map.map (\p el -> gto . SOP <$> p el) $ genericParseChoice' @a @(GDatatypeInfoOf a) @(GCode a)

class MapNamesToXML a => GenericParseChoice (a :: Type) (info :: DatatypeInfo) (code :: [[Type]]) where
  genericParseChoice' :: Map Name (AnnotatedElement i -> Parser i (NS (NP I) code))

instance
  GenericParseChoice' a constructors code =>
  GenericParseChoice a ('ADT _m _d constructors _s) code
  where
  genericParseChoice' = Map.fromList $ genericParseChoice'' @a @constructors @code

class MapNamesToXML a => GenericParseChoice' (a :: Type) (constructors :: [ConstructorInfo]) (code :: [[Type]]) where
  genericParseChoice'' :: [(Name, AnnotatedElement i -> Parser i (NS (NP I) code))]

instance MapNamesToXML a => GenericParseChoice' (a :: Type) '[] '[] where
  genericParseChoice'' = []

instance
  {-# OVERLAPPABLE #-}
  ( MapNamesToXML a,
    FromElement code,
    GenericParseChoice' a constructors codes,
    KnownSymbol constructor
  ) =>
  GenericParseChoice' (a :: Type) ('Constructor constructor ': constructors) ('[code] ': codes)
  where
  genericParseChoice'' =
    (mapNamesToXML @a . symbolVal $ Proxy @constructor, \el -> (\a -> Z (I a :* Nil)) <$> fromElement @code el) :
    map (second (fmap S .)) (genericParseChoice'' @a @constructors @codes)
-}
