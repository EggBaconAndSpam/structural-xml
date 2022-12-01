{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.XML.Generics.ResolveLabel where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.XML.Generics.Options
import GHC.TypeLits

data LabelClass = LAttribute | LContent | LElement | LSum

type family ResolveLabelClass (opts :: [Option]) (label :: Symbol) :: LabelClass where
  ResolveLabelClass opts label =
    FromJust
      ( OrMaybe
          ( OrMaybe
              (GuardPure (IsPrefix (OptionsAttributePrefix opts) label) 'LAttribute)
              (GuardPure (IsPrefix (OptionsContentPrefix opts) label) 'LContent)
          )
          ( OrMaybe
              (GuardPure (IsPrefix (OptionsSumPrefix opts) label) 'LSum)
              (GuardPure (IsPrefix (OptionsElementPrefix opts) label) 'LElement)
          )
      )

class ResolveLabelName (a :: Type) (opts :: [Option]) (label :: Symbol) where
  resolvedName :: String

instance ResolveLabelName' a opts (ResolveLabelClass opts label) label => ResolveLabelName a opts label where
  resolvedName = resolvedName' @a @opts @(ResolveLabelClass opts label) @label

class ResolveLabelName' (a :: Type) (opts :: [Option]) (labelClass :: LabelClass) (label :: Symbol) where
  resolvedName' :: String

instance
  ( ResolveNameModifier a (OptionsHasNameModifier opts),
    KnownSymbol label,
    KnownSymbol (OptionsElementPrefix opts)
  ) =>
  ResolveLabelName' a opts 'LElement label
  where
  resolvedName' =
    resolvedNameModifier @a @(OptionsHasNameModifier opts)
      . drop (length $ symbolVal (Proxy @(OptionsElementPrefix opts)))
      $ symbolVal (Proxy @label)

instance
  ( ResolveNameModifier a (OptionsHasNameModifier opts),
    KnownSymbol label,
    KnownSymbol (OptionsAttributePrefix opts)
  ) =>
  ResolveLabelName' a opts 'LAttribute label
  where
  resolvedName' =
    resolvedNameModifier @a @(OptionsHasNameModifier opts)
      . drop (length $ symbolVal (Proxy @(OptionsAttributePrefix opts)))
      $ symbolVal (Proxy @label)

instance ResolveLabelName' a opts 'LContent label where
  resolvedName' = "contents don't have names"

instance ResolveLabelName' a opts 'LSum label where
  resolvedName' = "the possible names of a sum element are given by it's NamedSum tags"

class ResolveNameModifier (a :: Type) (hasNameModifier :: Bool) where
  resolvedNameModifier :: String -> String

instance NameModifier a => ResolveNameModifier a 'True where
  resolvedNameModifier = nameModifier @a

instance ResolveNameModifier a 'False where
  resolvedNameModifier = id

type family FromJust (ma :: Maybe t) :: t where
  FromJust ('Just a) = a

type family FromMaybe (def :: t) (ma :: Maybe t) :: t where
  FromMaybe def 'Nothing = def
  FromMaybe _ ('Just a) = a

type family OrMaybe (x :: Maybe a) (y :: Maybe a) :: Maybe a where
  OrMaybe 'Nothing y = y
  OrMaybe x _ = x

type family GuardPure (b :: Bool) (a :: t) :: Maybe t where
  GuardPure 'False _ = 'Nothing
  GuardPure 'True a = 'Just a

-- Terminator hack...
type family IsPrefix (a :: Symbol) (b :: Symbol) :: Bool where
  IsPrefix a b = IsPrefix' (CmpSymbol a b) (CmpSymbol (AppendSymbol a Terminator) b)

type family IsPrefix' (l :: Ordering) (r :: Ordering) :: Bool where
  IsPrefix' 'LT 'GT = 'True
  IsPrefix' 'EQ 'GT = 'True
  IsPrefix' _ _ = 'False
