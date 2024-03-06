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
import GHC.TypeLits
import qualified Text.Regex.TDFA as Regex
import qualified Text.Regex.TDFA.String as Regex

data Restriction
  = FractionDigits Nat
  | TotalDigits Nat
  | Pattern Symbol
  | MinInclusive Nat
  | MinLength Nat
  | MaxLength Nat

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

-- Length is checked after stripping white space, but the white space is kept!
instance (KnownNat n) => Restrict ('MaxLength n) Text where
  restrict t = do
    unless (fromIntegral (T.length $ T.strip t) <= natVal (Proxy @n)) $ do
      Left $ "Text too long, expected at most " <> show (natVal $ Proxy @n) <> " characters."
    pure t

instance (KnownNat n) => Restrict ('MinLength n) Text where
  restrict t = do
    unless (fromIntegral (T.length $ T.strip t) >= natVal (Proxy @n)) $ do
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
      Left err -> const $ Left err
      Right regex -> \t ->
        if Regex.matchTest regex t
          then Left $ "Text does not match regex pattern " <> symbolVal (Proxy @s)
          else pure t
    where
      mkRegex =
        Regex.compile
          Regex.defaultCompOpt
          (Regex.defaultExecOpt {Regex.captureGroups = False})
          (symbolVal $ Proxy @s)

instance (FromContent a, MkRestricted rs a) => FromContent (Restricted rs a) where
  fromContent t i = do
    a <- fromContent t i
    either (Left . parserError i) pure $ mkRestricted @rs a

instance (FromElement a, MkRestricted rs a) => FromElement (Restricted rs a) where
  fromElement el@Element {info} = do
    a <- fromElement el
    either (Left . parserError info) pure $ mkRestricted @rs a
