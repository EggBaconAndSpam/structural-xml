{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Data.XML.BoundedList where

import Control.Monad
import Data.Decimal
import Data.Kind
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.XML
import Data.XML.Generic.GenericOrdered
import Data.XML.Generic.GenericUnordered
import Data.XML.Generic.GenericUnparseElement
import Data.XML.Generic.Names
import GHC.TypeLits
import Generics.SOP.Type.Metadata
import qualified Text.Regex.TDFA as Regex
import qualified Text.Regex.TDFA.Text as Regex
import Control.Monad.State
import Control.Monad.Except

data Restriction
  = FractionDigits Nat
  | TotalDigits Nat
  | Pattern Symbol
  | MinInclusive Nat
  | MinLength Nat
  | MaxLength Nat

instance
  ( MkRestricted rs code,
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
  ( MkRestricted rs code,
    GenericParseUnorderedField a ('FieldInfo field) 'ElementClass code
  ) =>
  GenericParseUnorderedField (a :: Type) ('FieldInfo field) 'ElementClass (Restricted rs code)
  where
  genericParseUnorderedField = do
    a <- genericParseUnorderedField @a @('FieldInfo field) @'ElementClass @code
    case mkRestricted a of
      Right b -> pure b
      Left err -> do
        Element {info} <- get
        throwError $ parserError info err

instance
  (GenericUnparseField a ('FieldInfo field) 'ElementClass code) =>
  GenericUnparseField a ('FieldInfo field) 'ElementClass (Restricted rs code)
  where
  genericUnparseField (Restricted a) = genericUnparseField @a @('FieldInfo field) @'ElementClass a

newtype Restricted (r :: [Restriction]) a = Restricted a
  deriving newtype (ToContent, ToElement, Eq, Ord, Show)

class Restrict (r :: Restriction) (a :: Type) where
  restrict :: a -> Either String a

instance (KnownNat n) => Restrict ('TotalDigits n) Decimal where
  restrict d = do
    let totalDigits = length . filter (/= '.') . show . normalizeDecimal $ abs d
    unless (fromIntegral totalDigits <= natVal (Proxy @n)) $ do
      Left $ "Number has too many digits, expected at most " <> show (natVal $ Proxy @n) <> " digits."
    pure d

instance (KnownNat n) => Restrict ('FractionDigits n) Decimal where
  restrict d = do
    let fractionDigits = decimalPlaces $ normalizeDecimal d
    unless (fromIntegral fractionDigits <= natVal (Proxy @n)) $ do
      Left $ "Number has too many fractional digits, expected at most " <> show (natVal $ Proxy @n) <> " digits."
    pure d

instance (KnownNat n) => Restrict ('MinInclusive n) Decimal where
  restrict d = do
    unless (d >= fromIntegral (natVal (Proxy @n))) $ do
      Left $ "Number is too small, expected at least " <> show (natVal $ Proxy @n) <> "."
    pure d

instance (KnownNat n) => Restrict ('MaxLength n) Text where
  restrict t = do
    unless (fromIntegral (T.length t) <= natVal (Proxy @n)) $ do
      Left $ "Text too long, expected at most " <> show (natVal $ Proxy @n) <> " characters."
    pure t

instance (KnownNat n) => Restrict ('MinLength n) Text where
  restrict t = do
    unless (fromIntegral (T.length t) >= natVal (Proxy @n)) $ do
      Left $ "Text too short, expected at least " <> show (natVal $ Proxy @n) <> " characters."
    pure t

instance
  {-# OVERLAPS #-}
  ( TypeError
      ( 'Text "The `Restrict ('MinLength n) String` instance is disallowed on purpose."
          ':<>: ('Text "Use `Text` instead or use a newtype wrapper.")
      )
  ) =>
  Restrict ('MinLength n) String
  where
  restrict = undefined

instance
  {-# OVERLAPS #-}
  ( TypeError
      ( 'Text "The `Restrict ('MaxLength n) String` instance is disallowed on purpose."
          ':<>: ('Text "Use `Text` instead or use a newtype wrapper.")
      )
  ) =>
  Restrict ('MaxLength n) String
  where
  restrict = undefined

instance (KnownNat n) => Restrict ('MinLength n) [a] where
  restrict t = do
    unless (fromIntegral (length t) >= natVal (Proxy @n)) $ do
      Left $ "List too short, expected at least " <> show (natVal $ Proxy @n) <> " items."
    pure t

instance (KnownNat n) => Restrict ('MaxLength n) [a] where
  restrict t = do
    unless (fromIntegral (length t) <= natVal (Proxy @n)) $ do
      Left $ "List too long, expected at most " <> show (natVal $ Proxy @n) <> " items."
    pure t

class MkRestricted (rs :: [Restriction]) (a :: Type) where
  mkRestricted :: a -> Either String (Restricted rs a)

instance MkRestricted '[] a where
  mkRestricted = pure . Restricted

instance
  (Restrict r a, MkRestricted rs a) =>
  MkRestricted (r ': rs) a
  where
  mkRestricted a = do
    a' <- restrict @r a
    (\(Restricted a'') -> Restricted a'') <$> mkRestricted @rs a'

instance (KnownSymbol s) => Restrict ('Pattern s) Text where
  restrict =
    case mkRegex of
      Left err -> const . Left $ "Invalid regex pattern \'" <> symbolVal (Proxy @s) <> "\": " <> err
      Right regex -> \t ->
        if Regex.matchTest regex t
          then pure t
          else Left $ "Text does not match regex pattern " <> symbolVal (Proxy @s) <> " : " <> T.unpack t
    where
      mkRegex =
        Regex.compile
          Regex.defaultCompOpt
          (Regex.defaultExecOpt {Regex.captureGroups = False})
          (T.pack $ symbolVal $ Proxy @s)

instance (FromContent a, MkRestricted rs a) => FromContent (Restricted rs a) where
  fromContent t i = do
    a <- fromContent t i
    either (Left . parserError i) pure $ mkRestricted @rs a

instance (FromElement a, MkRestricted rs a) => FromElement (Restricted rs a) where
  fromElement el@Element {info} = do
    a <- fromElement el
    either (Left . parserError info) pure $ mkRestricted @rs a
