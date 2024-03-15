module Data.XML.Generic.GenericDocument where

import Data.Kind
import Data.XML
import Data.XML.Generic.Names
import qualified GHC.Generics as GHC
import GHC.TypeLits
import Generics.SOP.BasicFunctors
import Generics.SOP.GGP
import Generics.SOP.NP
import Generics.SOP.NS
import Generics.SOP.Type.Metadata

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

class (MapNamesToXML a) => GenericUnparseDocument (a :: Type) (info :: DatatypeInfo) (code :: [[Type]]) where
  genericUnparseDocument' :: SOP I code -> Document

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

instance
  (MapNamesToXML a, ToElement root, KnownSymbol field) =>
  GenericUnparseDocument a ('ADT _m _d '[ 'Record _c '[ 'FieldInfo field]] _s) '[ '[root]]
  where
  genericUnparseDocument' (SOP x) =
    case x of
      Z (I y :* Nil) -> toRootElement (mapNameToElement @a @field) y
      S y -> case y of {}
