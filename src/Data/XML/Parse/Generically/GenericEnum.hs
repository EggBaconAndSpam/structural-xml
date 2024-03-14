{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.XML.Parse.Generically.GenericEnum where

import Data.Kind
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Data.XML
import Data.XML.Parse.Generically.Names
import qualified GHC.Generics as GHC
import GHC.TypeLits
import Generics.SOP.BasicFunctors
import Generics.SOP.GGP
import Generics.SOP.NP
import Generics.SOP.NS
import Generics.SOP.Type.Metadata

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

class GenericParseEnum (a :: Type) (info :: DatatypeInfo) (code :: [[Type]]) where
  genericParseEnum' :: Text -> i -> Parser i (SOP I code)

class GenericParseEnum' (a :: Type) (constructors :: [ConstructorInfo]) (code :: [[Type]]) where
  genericParseEnum'' :: Text -> i -> Either [String] (NS (NP I) code)

class GenericUnparseEnum (a :: Type) (info :: DatatypeInfo) (code :: [[Type]]) where
  genericUnparseEnum' :: SOP I code -> Text

class GenericUnparseEnum' (a :: Type) (constructors :: [ConstructorInfo]) (code :: [[Type]]) where
  genericUnparseEnum'' :: NS (NP I) code -> Text

instance
  (GenericParseEnum' a constructors code) =>
  GenericParseEnum a ('ADT _m _d constructors _s) code
  where
  genericParseEnum' t i = case genericParseEnum'' @a @constructors @code t i of
    Right a -> pure $ SOP a
    Left tags ->
      Left . parserError i $
        "Content \"" <> T.unpack t <> "\" didn't match any of the expected values " <> intercalate "|" tags

instance
  ( MapNamesToXML a,
    KnownSymbol name,
    GenericParseEnum' a constructors codes
  ) =>
  GenericParseEnum' a ('Constructor name ': constructors) ('[] ': codes)
  where
  genericParseEnum'' t i =
    let tag = mapNameToEnum @a @name
     in if t == T.pack tag
          then pure $ Z Nil
          else case genericParseEnum'' @a @constructors @codes t i of
            Right x -> pure $ S x
            Left tags -> Left $ tag : tags

instance
  ( FromContent code,
    Typeable code,
    GenericParseEnum' a constructors codes
  ) =>
  GenericParseEnum' a (_c ': constructors) ('[code] ': codes)
  where
  genericParseEnum'' t i =
    case fromContent t i of
      Right a -> pure $ Z (I a :* Nil)
      Left _err -> case genericParseEnum'' @a @constructors @codes t i of
        Right a -> pure $ S a
        Left tags -> Left $ "<" <> show (typeRep (Proxy @code)) <> ">" : tags

instance GenericParseEnum' a '[] '[] where
  genericParseEnum'' _ _ = Left []

genericUnparseEnum ::
  forall a.
  (GHC.Generic a, GFrom a, GenericUnparseEnum a (GDatatypeInfoOf a) (GCode a)) =>
  a ->
  Text
genericUnparseEnum a = genericUnparseEnum' @a @(GDatatypeInfoOf a) @(GCode a) $ gfrom a

instance
  (GenericUnparseEnum' a constructors code) =>
  GenericUnparseEnum a ('ADT _m _d constructors _s) code
  where
  genericUnparseEnum' (SOP x) = genericUnparseEnum'' @a @constructors @code x

instance
  ( MapNamesToXML a,
    KnownSymbol name,
    GenericUnparseEnum' a constructors codes
  ) =>
  GenericUnparseEnum' a ('Constructor name ': constructors) ('[] ': codes)
  where
  genericUnparseEnum'' (Z _) = T.pack $ mapNameToEnum @a @name
  genericUnparseEnum'' (S x) = genericUnparseEnum'' @a @constructors @codes x

instance GenericUnparseEnum' a '[] '[] where
  genericUnparseEnum'' x = case x of {}

instance
  ( GenericUnparseEnum' a constructors codes,
    ToContent code
  ) =>
  GenericUnparseEnum' a (_c ': constructors) ('[code] ': codes)
  where
  genericUnparseEnum'' (Z (I x :* Nil)) = toContent x
  genericUnparseEnum'' (S x) = genericUnparseEnum'' @a @constructors @codes x
