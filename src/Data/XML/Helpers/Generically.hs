{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.XML.Helpers.Generically where

import Data.Kind
import Data.XML
import Data.XML.Generics.Options
import Data.XML.Generics.Record
import Data.XML.Generics.Shape
import Data.XML.Generics.Sum
import qualified GHC.Generics as GHC
import Generics.SOP (All, Generic (..), HasDatatypeInfo (..), SListI)
import Generics.SOP.GGP

newtype Generically (opts :: [Option]) (a :: Type) = Generically a deriving newtype (GHC.Generic)

instance (GFrom a, GHC.Generic a, GTo a, (All SListI (GCode a))) => Generic (Generically opts a) where
  type Code (Generically opts a) = GCode a
  from (Generically a) = gfrom a
  to = Generically . gto

instance
  (Generic (Generically opts a), GDatatypeInfo (Generically opts a), GCode (Generically opts a) ~ Code (Generically opts a)) =>
  HasDatatypeInfo (Generically opts a)
  where
  type DatatypeInfoOf (Generically opts a) = GDatatypeInfoOf a

  datatypeInfo = gdatatypeInfo

instance
  ( Generic (Generically opts a),
    GenericallyFromElement opts a (ComputeShape (DatatypeInfoOf (Generically opts a)) (Code (Generically opts a)))
  ) =>
  FromElement (Generically opts a)
  where
  fromElement = genericallyFromElement @opts @a @(ComputeShape (DatatypeInfoOf (Generically opts a)) (Code (Generically opts a)))

class GenericallyFromElement (opts :: [Option]) (a :: Type) (shape :: Shape) where
  genericallyFromElement :: Element -> Either ParserError (Generically opts a)

instance
  ( Generic (Generically opts a),
    RecordFromElement a opts (DatatypeInfoOf (Generically opts a)) (Code (Generically opts a))
  ) =>
  GenericallyFromElement opts a 'RecordShape
  where
  genericallyFromElement =
    fmap (to @(Generically opts a))
      . recordFromElement @a @opts @(DatatypeInfoOf (Generically opts a)) @(Code (Generically opts a))

instance
  ( Generic (Generically opts a),
    SumFromElement a opts (DatatypeInfoOf (Generically opts a)) (Code (Generically opts a))
  ) =>
  GenericallyFromElement opts a 'SumShape
  where
  genericallyFromElement =
    fmap (to @(Generically opts a))
      . sumFromElement @a @opts @(DatatypeInfoOf (Generically opts a)) @(Code (Generically opts a))
