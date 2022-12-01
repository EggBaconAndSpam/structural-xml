{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.XML.Generics.Record where

import Control.Monad.Error.Class (MonadError)
import Data.Data
import Data.Kind
import Data.String (IsString (..))
import Data.XML
import Data.XML.Generics.NamedSum
import Data.XML.Generics.Options
import Data.XML.Generics.ResolveLabel
import GHC.TypeLits
import Generics.SOP.BasicFunctors
import Generics.SOP.NP
import Generics.SOP.NS
import Generics.SOP.Type.Metadata

class RecordFromElement (a :: Type) (options :: [Option]) (info :: DatatypeInfo) (code :: [[Type]]) where
  recordFromElement :: Element -> Either ParserError (SOP I code)

instance
  RecordConstructorFromElement a opts info code =>
  RecordFromElement a opts ('Newtype _module _datatype info) '[code]
  where
  recordFromElement = fmap (SOP . Generics.SOP.NS.Z) . recordConstructorFromElement @a @opts @info @code

instance
  RecordConstructorFromElement a opts info code =>
  RecordFromElement a opts ('ADT _module _datatype '[info] _strictness) '[code]
  where
  recordFromElement = fmap (SOP . Generics.SOP.NS.Z) . recordConstructorFromElement @a @opts @info @code

class RecordConstructorFromElement (a :: Type) (options :: [Option]) (info :: ConstructorInfo) (code :: [Type]) where
  recordConstructorFromElement :: Element -> Either ParserError (NP I code)

instance
  RecordFieldsFromElement a opts info code =>
  RecordConstructorFromElement a opts ('Record _name info) code
  where
  recordConstructorFromElement = parseOrderedElement (recordFieldsFromElement @a @opts @info @code)

class RecordFieldsFromElement (a :: Type) (options :: [Option]) (info :: [FieldInfo]) (code :: [Type]) where
  recordFieldsFromElement :: (AttributeConsumer m, ElementConsumer m, MonadError ParserError m) => m (NP I code)

instance RecordFieldsFromElement a opts '[] '[] where
  recordFieldsFromElement = pure Nil

instance
  ( RecordFieldsFromElement a opts info ts,
    RecordFieldFromElement a opts (ResolveLabelClass opts label) ('FieldInfo label) t
  ) =>
  RecordFieldsFromElement a opts ('FieldInfo label ': info) (t ': ts)
  where
  recordFieldsFromElement =
    (\t ts -> I t :* ts) <$> recordFieldFromElement @a @opts @(ResolveLabelClass opts label) @('FieldInfo label) @t
      <*> recordFieldsFromElement @a @opts @info @ts

class
  RecordFieldFromElement
    (a :: Type)
    (options :: [Option])
    (labelClass :: LabelClass)
    (label :: FieldInfo)
    (t :: Type)
  where
  recordFieldFromElement :: (AttributeConsumer m, ElementConsumer m, MonadError ParserError m) => m t

instance
  (FromContent t, ResolveLabelName a opts label) =>
  RecordFieldFromElement a opts 'LAttribute ('FieldInfo label) t
  where
  recordFieldFromElement = consumeAttribute (fromString $ resolvedName @a @opts @label)

instance
  {-# OVERLAPPING #-}
  (FromContent t, ResolveLabelName a opts label) =>
  RecordFieldFromElement a opts 'LAttribute ('FieldInfo label) (Maybe t)
  where
  recordFieldFromElement = consumeOptionalAttribute (fromString $ resolvedName @a @opts @label)

instance
  {-# OVERLAPPING #-}
  FromChoiceElement t =>
  RecordFieldFromElement a opts 'LSum ('FieldInfo _label) t
  where
  recordFieldFromElement = consumeChoiceElement

class
  RecordSumFieldFromElement
    (options :: [Option])
    (t :: [(Symbol, Type)])
  where
  recordSumFieldFromElement :: (AttributeConsumer m, ElementConsumer m, MonadError ParserError m) => m (Maybe (NamedSum t))

instance RecordSumFieldFromElement opts '[] where
  recordSumFieldFromElement = pure Nothing

instance
  (RecordSumFieldFromElement opts ts, FromElement t, KnownSymbol label) =>
  RecordSumFieldFromElement opts ('(label, t) ': ts)
  where
  recordSumFieldFromElement = do
    consumeElementOrAbsent (fromString $ symbolVal (Proxy @label)) >>= \case
      Nothing ->
        fmap (fmap Data.XML.Generics.NamedSum.S) $ recordSumFieldFromElement @opts @ts
      Just t -> pure $ Just (Data.XML.Generics.NamedSum.Z t)

instance
  (FromElement t, ResolveLabelName a opts label) =>
  RecordFieldFromElement a opts 'LElement ('FieldInfo label) t
  where
  recordFieldFromElement = consumeElement (fromString $ resolvedName @a @opts @label)

instance
  {-# OVERLAPPING #-}
  (FromElement t, ResolveLabelName a opts label) =>
  RecordFieldFromElement a opts 'LElement ('FieldInfo label) [t]
  where
  recordFieldFromElement =
    consumeElementOrAbsent (fromString $ resolvedName @a @opts @label) >>= \case
      Nothing -> pure []
      Just t -> (t :) <$> recordFieldFromElement @a @opts @'LElement @('FieldInfo label) @[t]

instance
  {-# OVERLAPPING #-}
  OptionalElementFieldFromElement a opts (OptionsNothingEncoding opts) ('FieldInfo label) t =>
  RecordFieldFromElement a opts 'LElement ('FieldInfo label) (Maybe t)
  where
  recordFieldFromElement = optionalElementFieldFromElement @a @opts @(OptionsNothingEncoding opts) @('FieldInfo label) @t

class
  OptionalElementFieldFromElement
    (a :: Type)
    (options :: [Option])
    (mNothingEncoding :: Maybe NothingEncoding)
    (label :: FieldInfo)
    (t :: Type)
  where
  optionalElementFieldFromElement :: (AttributeConsumer m, ElementConsumer m, MonadError ParserError m) => m (Maybe t)

instance
  TypeError ('Text "You need to specify a NothingEncoding if you use Maybe types.") =>
  OptionalElementFieldFromElement a opts 'Nothing label t
  where
  optionalElementFieldFromElement = error "unreachable"

instance
  (FromElement t, ResolveLabelName a opts label) =>
  OptionalElementFieldFromElement a opts ('Just 'OmitNothing) ('FieldInfo label) t
  where
  optionalElementFieldFromElement = consumeElementOrAbsent (fromString $ resolvedName @a @opts @label)

instance
  (FromElement t, ResolveLabelName a opts label) =>
  OptionalElementFieldFromElement a opts ('Just 'EmptyNothing) ('FieldInfo label) t
  where
  optionalElementFieldFromElement = consumeElementOrEmpty (fromString $ resolvedName @a @opts @label)
