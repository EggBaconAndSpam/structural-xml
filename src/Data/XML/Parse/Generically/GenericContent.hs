-- todo: try out overlappable instead of overlapping
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.XML.Parse.Generically.GenericContent where

import Control.Monad.Except
import Control.Monad.State
import Data.Kind
import Data.XML
import Data.XML.Parse.Generically.GenericUnparseElement
import Data.XML.Parse.Generically.Names
import Data.XML.Parse.Ordered
import qualified GHC.Generics as GHC
import GHC.TypeLits
import Generics.SOP.BasicFunctors
import Generics.SOP.GGP
import Generics.SOP.NP
import Generics.SOP.NS
import Generics.SOP.Type.Metadata

newtype GenericContent a = GenericContent a

instance
  (GHC.Generic a, GTo a, GenericParseContent a (GDatatypeInfoOf a) (GCode a)) =>
  FromElement (GenericContent a)
  where
  fromElement el = GenericContent <$> genericParseContent el

instance
  (GHC.Generic a, GFrom a, GenericUnparseContent a (GDatatypeInfoOf a) (GCode a)) =>
  ToElement (GenericContent a)
  where
  toElement (GenericContent a) = genericUnparseContent a

genericParseContent ::
  forall a i.
  (GHC.Generic a, GTo a, GenericParseContent a (GDatatypeInfoOf a) (GCode a)) =>
  AnnotatedElement i ->
  Parser i a
genericParseContent el = gto <$> genericParseContent' @a @(GDatatypeInfoOf a) @(GCode a) el

genericUnparseContent ::
  forall a.
  (GHC.Generic a, GFrom a, GenericUnparseContent a (GDatatypeInfoOf a) (GCode a)) =>
  a ->
  Element
genericUnparseContent x = genericUnparseContent' @a @(GDatatypeInfoOf a) @(GCode a) $ gfrom x

class GenericParseContent (a :: Type) (info :: DatatypeInfo) (code :: [[Type]]) where
  genericParseContent' :: AnnotatedElement i -> Parser i (SOP I code)

class GenericParseContent' (a :: Type) (fields :: [FieldInfo]) (code :: [Type]) where
  genericParseContent'' :: OrderedM i (NP I code)

class GenericUnparseContent (a :: Type) (info :: DatatypeInfo) (code :: [[Type]]) where
  genericUnparseContent' :: SOP I code -> Element

class GenericUnparseContent' (a :: Type) (fields :: [FieldInfo]) (code :: [Type]) where
  genericUnparseContent'' :: NP I code -> Element

instance
  (GenericUnparseContent' a fields code) =>
  GenericUnparseContent a ('ADT _m _d '[ 'Record _c fields] _s) '[code]
  where
  genericUnparseContent' (SOP x) =
    case x of
      Z y -> genericUnparseContent'' @a @fields @code y
      S y -> case y of {}

instance GenericUnparseContent' a '[] '[] where
  genericUnparseContent'' Nil = mempty

instance
  ( GenericUnparseField a field (ClassifyContentLabel field) code,
    GenericUnparseContent' a fields codes
  ) =>
  GenericUnparseContent' a (field ': fields) (code ': codes)
  where
  genericUnparseContent'' (I x :* xs) =
    genericUnparseField @a @field @(ClassifyContentLabel field) @code x
      <> genericUnparseContent'' @a @fields @codes xs

instance
  (GenericParseContent' a fields code) =>
  GenericParseContent a ('ADT _m _d '[ 'Record _c fields] _s) '[code]
  where
  genericParseContent' el = SOP . Z <$> parseOrderedElement (genericParseContent'' @a @fields @code) el

instance GenericParseContent' (a :: Type) '[] '[] where
  genericParseContent'' = pure Nil

instance
  ( GenericParseContentField a field (ClassifyContentLabel field) code,
    GenericParseContent' a fields codes
  ) =>
  GenericParseContent' (a :: Type) (field ': fields) (code ': codes)
  where
  genericParseContent'' = do
    x <- genericParseContentField @a @field @(ClassifyContentLabel field) @code
    rest <- genericParseContent'' @a @fields @codes
    pure (I x :* rest)

class GenericParseContentField (a :: Type) (field :: FieldInfo) (l :: LabelClass) (code :: Type) where
  genericParseContentField :: OrderedM i code -- ContentM?

instance
  {-# OVERLAPPABLE #-}
  ( MapNamesToXML a,
    FromContent code,
    KnownSymbol field
  ) =>
  GenericParseContentField (a :: Type) ('FieldInfo field) 'AttributeClass code
  where
  genericParseContentField = do
    consumeAttribute (mapNameToAttribute @a @field)

instance
  ( MapNamesToXML a,
    FromContent code,
    KnownSymbol field
  ) =>
  GenericParseContentField (a :: Type) ('FieldInfo field) 'AttributeClass (Maybe code)
  where
  genericParseContentField = do
    consumeOptionalAttribute (mapNameToAttribute @a @field)

instance (FromContent code) => GenericParseContentField (a :: Type) fieldinfo 'ContentClass code where
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
