{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Data.XML.Camt53 where

import Control.Applicative
import Control.Monad
import Data.Char (isUpper)
import Data.Decimal
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time
import Data.XML (Leftovers, ReadShowXmlElement (..), decodeDocument)
import Data.XML.ISO20022.DerivingViaHelpers
import Data.XML.ISO20022.NameMangling
import Data.XML.ISO20022.Restricted
import Data.XML.Parse.Types
import Data.XML.Unparse hiding (Name)
import Data.XML.XSD2
import qualified Data.XML.XSD2 as Xsd
import GHC.Generics (Generic)
import Language.Haskell.TH
import Optics

emptyRestriction :: Xsd.Restriction -> Xsd.Restriction
emptyRestriction Restriction {attr_base} =
  Restriction
    { attr_base,
      minLength = Nothing,
      maxLength = Nothing,
      pattern = Nothing,
      fractionDigits = Nothing,
      totalDigits = Nothing,
      minInclusive = Nothing,
      enumeration = []
    }

noBang :: Bang
noBang = Bang NoSourceUnpackedness NoSourceStrictness

mkName' :: Text -> Name
mkName' = mkName . Text.unpack

{-
todo: drop xsi attributes?
xsi:schemaLocation="urn:iso:std:iso:20022:tech:xsd:camt.053.001.02
  camt.053.001.02.xsd" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance

For each attribute information item in the element information item's
[attributes] excepting those whose [namespace name] is identical to
http://www.w3.org/2001/XMLSchema-instance and whose [local name] is one of type,
nil, schemaLocation or noNamespaceSchemaLocation, the appropriate case among the
following must be true:
  -}

unmangledElementName :: Text -> Name
unmangledElementName t =
  let uname = unmangleName $ Text.unpack t
   in if
        | uname == "type" -> mkName "elem_type"
        | i : _ <- uname, isUpper i -> mkName $ "elem_" <> uname
        | otherwise -> mkName uname

qCamt53Schema :: Q [Dec]
qCamt53Schema = do
  camt53xsd <- runIO $ Text.readFile "/home/frederikramcke/Documents/structural-xml/camt.053.001.02.xsd"
  case decodeDocument camt53xsd of
    Left err -> error err
    Right SchemaDoc {schema} -> reifySchema schema

reifySchema :: Schema -> Q [Dec]
reifySchema s = do
  let [documentRoot] = s ^. #element
  let mangledNames = schemaElementNames s
  unless (all (\n -> let n' = Text.unpack n in mangleName (unmangleName n') == n') mangledNames) $
    error "mangling is wrong!"
  simpleTypeDecs <- fmap concat . mapM reifySimpleType $ s ^. #simpleType
  complexTypeDecs <- fmap concat . mapM reifyComplexType $ s ^. #complexType
  pure $ schema_document documentRoot : simpleTypeDecs <> complexTypeDecs

schema_document :: Element -> Dec
schema_document e =
  let typeName = mkName "DocumentRoot"
   in DataD
        []
        typeName
        []
        Nothing
        [RecC typeName [(mkName' $ "elem_" <> e ^. #attr_name, noBang, elementType e)]]
        [ DerivClause (Just StockStrategy) [ConT ''Generic],
          DerivClause (Just $ ViaStrategy (AppT (ConT ''Camt53Document) (ConT typeName))) [ConT ''FromDocument, ConT ''ToDocument]
        ]

reifySimpleType :: SimpleType -> Q [Dec]
reifySimpleType st = do
  let typeName = mkName' $ st ^. #attr_name
  hasOverride <- fmap isJust . lookupTypeName . Text.unpack $ st ^. #attr_name
  if hasOverride
    then pure []
    else
      pure . pure . fromMaybe (error $ "unhandled SimpleType" <> show (ReadShowXmlElement st)) $
        simpleType_string_pattern typeName st
          <|> simpleType_string_length typeName st
          <|> simpleType_string_enumeration typeName st
          <|> simpleType_decimal typeName st

reifyComplexType :: ComplexType -> Q [Dec]
reifyComplexType ct = do
  let typeName = mkName' $ ct ^. #attr_name
  hasOverride <- fmap isJust . lookupTypeName . Text.unpack $ ct ^. #attr_name
  if hasOverride
    then pure []
    else
      pure . pure $
        fromMaybe (error $ "unhandled ComplexType" <> show (ReadShowXmlElement ct)) $
          complexType_sequence_elements typeName ct
            <|> complexType_sequence_any typeName ct
            <|> complexType_sequence_choice typeName ct
            <|> complexType_choice typeName ct
            <|> complexType_content_extension typeName ct

simpleType_string_pattern :: Name -> SimpleType -> Maybe Dec
simpleType_string_pattern typeName st
  | r <- st ^. #restriction,
    r ^. #attr_base == "xs:string",
    Just (Value pattern) <- r ^. #pattern,
    r {pattern = Nothing} == emptyRestriction r =
      let accessorName = mkName' $ "un" <> st ^. #attr_name
          innerType =
            AppT
              ( AppT
                  (ConT ''Restricted)
                  ( AppT
                      ( AppT
                          PromotedConsT
                          (AppT (PromotedT 'Pattern) (LitT (StrTyLit $ Text.unpack pattern)))
                      )
                      PromotedNilT
                  )
              )
              (ConT ''Text)
       in Just $
            NewtypeD
              []
              typeName
              []
              Nothing
              (RecC typeName [(accessorName, noBang, innerType)])
              [DerivClause (Just NewtypeStrategy) [ConT ''FromContent, ConT ''FromElement, ConT ''ToContent, ConT ''ToElement]]
  | otherwise = Nothing

simpleType_string_length :: Name -> SimpleType -> Maybe Dec
simpleType_string_length typeName st
  | r <- st ^. #restriction,
    r ^. #attr_base == "xs:string",
    Just (Value l) <- r ^. #minLength,
    Just (Value u) <- r ^. #maxLength,
    r {minLength = Nothing, maxLength = Nothing} == emptyRestriction r =
      let accessorName = mkName' $ "un" <> st ^. #attr_name
          innerType =
            AppT
              ( AppT
                  (ConT ''Restricted)
                  ( AppT
                      ( AppT
                          PromotedConsT
                          (AppT (PromotedT 'MinLength) (LitT (NumTyLit $ fromIntegral l)))
                      )
                      ( AppT
                          ( AppT
                              PromotedConsT
                              (AppT (PromotedT 'MaxLength) (LitT (NumTyLit $ fromIntegral u)))
                          )
                          PromotedNilT
                      )
                  )
              )
              (ConT ''Text)
       in Just $
            NewtypeD
              []
              typeName
              []
              Nothing
              (RecC typeName [(accessorName, noBang, innerType)])
              [DerivClause (Just NewtypeStrategy) [ConT ''FromContent, ConT ''FromElement, ConT ''ToContent, ConT ''ToElement]]
  | otherwise = Nothing

simpleType_string_enumeration :: Name -> SimpleType -> Maybe Dec
simpleType_string_enumeration typeName st
  | r <- st ^. #restriction,
    r ^. #attr_base == "xs:string",
    not . null $ r ^. #enumeration,
    r {enumeration = []} == emptyRestriction r =
      let conNames =
            map (\v -> NormalC (mkName' $ st ^. #attr_name <> "_" <> v ^. #attr_value) []) $
              r ^. #enumeration
       in Just $
            DataD
              []
              typeName
              []
              Nothing
              conNames
              [ DerivClause (Just StockStrategy) [ConT ''Generic],
                DerivClause (Just $ ViaStrategy (AppT (ConT ''Camt53Enum) (ConT typeName))) [ConT ''FromContent, ConT ''FromElement, ConT ''ToContent, ConT ''ToElement]
              ]
  | otherwise = Nothing

simpleType_decimal :: Name -> SimpleType -> Maybe Dec
simpleType_decimal typeName st
  | r <- st ^. #restriction,
    r ^. #attr_base == "xs:decimal",
    r {fractionDigits = Nothing, totalDigits = Nothing, minInclusive = Nothing}
      == emptyRestriction r =
      let accessorName = mkName' $ "un" <> st ^. #attr_name
          restrictions :: [Type]
          restrictions =
            ( do
                Value n <- maybeToList $ r ^. #fractionDigits
                pure (AppT (PromotedT 'FractionDigits) (LitT (NumTyLit $ fromIntegral n)))
            )
              <> ( do
                     Value n <- maybeToList $ r ^. #minInclusive
                     pure (AppT (PromotedT 'MinInclusive) (LitT (NumTyLit $ fromIntegral n)))
                 )
              <> ( do
                     Value n <- maybeToList $ r ^. #totalDigits
                     pure (AppT (PromotedT 'TotalDigits) (LitT (NumTyLit $ fromIntegral n)))
                 )
          x = foldr (AppT . AppT PromotedConsT) PromotedNilT restrictions
          innerType = AppT (AppT (ConT ''Restricted) x) (ConT ''Decimal)
       in Just $
            NewtypeD
              []
              typeName
              []
              Nothing
              (RecC typeName [(accessorName, noBang, innerType)])
              [DerivClause (Just NewtypeStrategy) [ConT ''FromContent, ConT ''FromElement, ConT ''ToContent, ConT ''ToElement]]
  | otherwise = Nothing

elementType :: Element -> Type
elementType Element {..} =
  if
    | isNothing attr_minOccurs,
      isNothing attr_maxOccurs ->
        innerTyp
    | Just (Int 1) == attr_minOccurs,
      isNothing attr_maxOccurs || Just Unbounded == attr_maxOccurs ->
        AppT (ConT ''NonEmpty) innerTyp
    | Just (Int 0) == attr_minOccurs,
      isNothing attr_maxOccurs || Just Unbounded == attr_maxOccurs ->
        AppT ListT innerTyp
    | Just (Int 0) == attr_minOccurs,
      Just (Int 1) == attr_maxOccurs ->
        AppT (ConT ''Maybe) innerTyp
    | Just (Int 0) == attr_minOccurs,
      Just (Int n) <- attr_maxOccurs ->
        AppT
          ( AppT
              (ConT ''Restricted)
              ( AppT
                  ( AppT
                      PromotedConsT
                      (AppT (PromotedT 'MaxLength) (LitT (NumTyLit $ fromIntegral n)))
                  )
                  PromotedNilT
              )
          )
          (AppT ListT innerTyp)
    | otherwise -> error $ "unhandled" <> show (ReadShowXmlElement Element {..})
  where
    innerTyp = ConT $ mkName' attr_type

complexType_sequence_elements :: Name -> ComplexType -> Maybe Dec
complexType_sequence_elements typeName ct
  | ComplexTypeSequence {sequence = SequenceElements {element = es}} <- ct =
      let mkField el = (unmangledElementName $ el ^. #attr_name, noBang, elementType el)
       in Just $
            DataD
              []
              typeName
              []
              Nothing
              [RecC typeName (map mkField es)]
              [ DerivClause (Just StockStrategy) [ConT ''Generic],
                DerivClause (Just $ ViaStrategy (AppT (ConT ''Camt53Element) (ConT typeName))) [ConT ''FromElement, ConT ''ToElement]
              ]
  | otherwise = Nothing

complexType_sequence_any :: Name -> ComplexType -> Maybe Dec
complexType_sequence_any typeName ct
  | ComplexTypeSequence {sequence = SequenceAny {}} <- ct =
      Just $
        DataD
          []
          typeName
          []
          Nothing
          [RecC typeName [(mkName "any", noBang, ConT ''Leftovers)]]
          [ DerivClause (Just StockStrategy) [ConT ''Generic],
            DerivClause (Just $ ViaStrategy (AppT (ConT ''Camt53Element) (ConT typeName))) [ConT ''FromElement, ConT ''ToElement]
          ]
  | otherwise = Nothing

-- camt053.02
complexType_sequence_choice :: Name -> ComplexType -> Maybe Dec
complexType_sequence_choice typeName ct
  | ComplexTypeSequence {sequence = SequenceChoice {choice = Choice {element = e}}} <- ct =
      let mkConstructor :: Element -> Con
          mkConstructor element' =
            RecC
              (mkName' $ ct ^. #attr_name <> element' ^. #attr_name)
              [(unmangledElementName $ element' ^. #attr_name, noBang, elementType element')]
       in Just $
            DataD
              []
              typeName
              []
              Nothing
              (map mkConstructor e)
              [ DerivClause (Just StockStrategy) [ConT ''Generic],
                DerivClause (Just $ ViaStrategy (AppT (ConT ''Camt53Element) (ConT typeName))) [ConT ''FromElement, ConT ''ToElement]
              ]
  | otherwise = Nothing

complexType_choice :: Name -> ComplexType -> Maybe Dec
complexType_choice typeName ct
  | ComplexTypeChoice {choice = Choice {element = e}} <- ct =
      let mkConstructor :: Element -> Con
          mkConstructor element' =
            RecC
              (mkName' $ ct ^. #attr_name <> element' ^. #attr_name)
              [(unmangledElementName $ element' ^. #attr_name, noBang, elementType element')]
       in Just $
            DataD
              []
              typeName
              []
              Nothing
              (map mkConstructor e)
              [ DerivClause (Just StockStrategy) [ConT ''Generic],
                DerivClause (Just $ ViaStrategy (AppT (ConT ''Camt53Element) (ConT typeName))) [ConT ''FromElement, ConT ''ToElement]
              ]
  | otherwise = Nothing

complexType_content_extension :: Name -> ComplexType -> Maybe Dec
complexType_content_extension typeName ct
  | ComplexTypeContent {simpleContent = SimpleContent {extension = e}} <- ct,
    e ^. #attribute % #attr_use == Required =
      let attributeField =
            ( mkName $ "attr_" <> unmangleName (Text.unpack $ e ^. #attribute % #attr_name),
              noBang,
              ConT . mkName' $ e ^. #attribute % #attr_type
            )
          contentField = (mkName "content", noBang, ConT $ mkName' $ e ^. #attr_base)
       in Just $
            DataD
              []
              typeName
              []
              Nothing
              [RecC typeName [attributeField, contentField]]
              [ DerivClause (Just StockStrategy) [ConT ''Generic],
                DerivClause (Just $ ViaStrategy (AppT (ConT ''Camt53Content) (ConT typeName))) [ConT ''FromElement, ConT ''ToElement]
              ]
  | otherwise = Nothing
