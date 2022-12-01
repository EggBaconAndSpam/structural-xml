{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.XML.Generics.Options where

import GHC.TypeLits

-- todo: define namespace
-- todo: ignore namespace entirely? -> Preprocessing of document
-- todo: check options are consistent
-- todo: NameModifier -> ModifyNames?
-- could also have "Rename" for single renames (e.g. _type -> type)
data Option
  = NothingElementEncoding NothingEncoding
  | NameModifier
  | ContentPrefix Symbol
  | AttributePrefix Symbol
  | SumPrefix Symbol
  | ElementPrefix Symbol
  | Unordered {- TODO -}
  | KeepWhitespace {- TODO -}
  | Rename Symbol Symbol {- TODO -}

data NothingEncoding = OmitNothing | EmptyNothing

-- could move this into 'Maybe' instance?
type family OptionsNothingEncoding (options :: [Option]) :: Maybe NothingEncoding where
  OptionsNothingEncoding '[] = 'Nothing
  OptionsNothingEncoding ('NothingElementEncoding enc ': _opts) = 'Just enc
  OptionsNothingEncoding (_ ': opts) = OptionsNothingEncoding opts

type family OptionsHasNameModifier (options :: [Option]) :: Bool where
  OptionsHasNameModifier ('NameModifier ': _) = 'True
  OptionsHasNameModifier (_ ': opts) = OptionsHasNameModifier opts
  OptionsHasNameModifier '[] = 'False

-- The 'terminator' consists of the last valid unicode character (that is also
-- guaranteed to never be used for encoding, i.e. it will never appear in a
-- Haskell name). We use that fact that `a` is a prefix of `b` iff `a < b` and
-- `(a ++ Terminator) > b`.
type Terminator = "\x10FFFF" :: Symbol

type family OptionsContentPrefix (options :: [Option]) :: Symbol where
  OptionsContentPrefix ('ContentPrefix label ': _) = label
  OptionsContentPrefix '[] = Terminator
  OptionsContentPrefix (_ ': opts) = OptionsContentPrefix opts

type family OptionsAttributePrefix (options :: [Option]) :: Symbol where
  OptionsAttributePrefix ('AttributePrefix label ': _) = label
  OptionsAttributePrefix '[] = Terminator
  OptionsAttributePrefix (_ ': opts) = OptionsAttributePrefix opts

type family OptionsSumPrefix (options :: [Option]) :: Symbol where
  OptionsSumPrefix ('SumPrefix label ': _) = label
  OptionsSumPrefix '[] = Terminator
  OptionsSumPrefix (_ ': opts) = OptionsSumPrefix opts

type family OptionsElementPrefix (options :: [Option]) :: Symbol where
  OptionsElementPrefix ('ElementPrefix label ': _) = label
  OptionsElementPrefix '[] = "" -- by default everything is an element
  OptionsElementPrefix (_ ': opts) = OptionsElementPrefix opts

class NameModifier a where
  nameModifier :: String -> String
