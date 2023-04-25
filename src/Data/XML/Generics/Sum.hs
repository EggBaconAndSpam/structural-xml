{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.XML.Generics.Sum where

import Control.Monad.Error.Class (MonadError)
import Data.Kind
import Data.String (IsString (..))
import Data.XML
import Data.XML.Generics.Options
import Data.XML.Generics.ResolveLabel
import Generics.SOP.BasicFunctors
import Generics.SOP.NP
import Generics.SOP.NS
import Generics.SOP.Type.Metadata

{-
data AccountIdentification4Choice = IBAN IBAN2007Identifier | Othr GenericAccountIdentification1

<=>

<xs:complexType name="AccountIdentification4Choice">
  <xs:choice>
    <xs:element name="IBAN" type="IBAN2007Identifier"/>
    <xs:element name="Othr" type="GenericAccountIdentification1"/>
  </xs:choice>
</xs:complexType>
-}

class SumFromElement (a :: Type) (options :: [Option]) (info :: DatatypeInfo) (code :: [[Type]]) where
  sumFromElement :: Element -> Either ParserError (SOP I code)

instance SumFromElement a opts ('ADT _module _type '[info] '[]) code => SumFromElement a opts ('Newtype _module _type info) code where
  sumFromElement = sumFromElement @a @opts @('ADT _module _type '[info] '[]) @code

instance SumConstructorsFromElement a opts info code => SumFromElement a opts ('ADT _module _type info _strictness) code where
  sumFromElement = parseOrderedElement $ do
    sumConstructorsFromElement @a @opts @info @code >>= \case
      Nothing -> throwParserError "No constructor matched"
      Just c -> pure $ SOP c

class SumConstructorsFromElement (a :: Type) (options :: [Option]) (info :: [ConstructorInfo]) (code :: [[Type]]) where
  sumConstructorsFromElement :: (AttributeConsumer m, ElementConsumer m, MonadError ParserError m) => m (Maybe (NS (NP I) code))

instance
  (FromElement t, ResolveLabelName a opts label, SumConstructorsFromElement a opts info code) =>
  SumConstructorsFromElement a opts ('Constructor label ': info) ('[t] ': code)
  where
  sumConstructorsFromElement = do
    let name = fromString $ resolvedName @a @opts @label
    consumeElementOrAbsent name >>= \case
      Nothing ->
        fmap (fmap S) $ sumConstructorsFromElement @a @opts @info @code
      Just t -> pure $ Just (Z (I t :* Nil))

instance SumConstructorsFromElement a opts '[] '[] where
  sumConstructorsFromElement = pure Nothing
