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
import Data.Char (isUpper, toLower, toUpper)
import Data.Decimal
import Data.Foldable
import Data.List (inits)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time
import Data.Tuple (swap)
import Data.XML (ContentElement (..), Leftovers, ReadShowXmlElement (..), decodeDocument)
import Data.XML.BoundedList
import Data.XML.Parse.Generically.Labelled
import Data.XML.Parse.Ordered (AnyElement)
import Data.XML.Parse.Types
import qualified Data.XML.Types as XML
import Data.XML.XSD2
import qualified Data.XML.XSD2 as Xsd
import GHC.Generics (Generic)
import Generics.SOP.GGP (GTo)
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

unmanglingDictionary :: Map String String
unmanglingDictionary = Map.fromList . map swap $ Map.toList manglingDictionary

-- longest match takes precedence
manglingDictionary :: Map String String
manglingDictionary =
  Map.fromList
    [ ("acceptance", "Accptnc"),
      ("account", "Acct"),
      ("actual", "Actl"),
      ("action", "Actn"),
      ("activity", "Actvty"),
      ("additional", "Addtl"),
      ("adjustment", "Adjstmnt"),
      ("administration", "Admstn"),
      ("administrator", "Admstr"),
      ("address", "Adr"),
      ("aggregated", "Aggtd"),
      ("agent", "Agt"),
      ("agents", "Agts"),
      ("amount", "Amt"),
      ("amortised", "Amtsd"),
      ("announced", "Anncd"),
      ("applied", "Apld"),
      ("approval", "Apprvl"),
      ("attendance", "Attndnc"),
      ("attendant", "Attndnt"),
      ("authentication", "Authntcn"),
      ("authorisation", "Authstn"),
      ("availability", "Avlbty"),
      ("balance", "Bal"),
      ("boundary", "Bdry"),
      ("business", "Biz"),
      ("bank", "Bk"),
      ("building", "Bldg"),
      ("booking", "Bookg"),
      ("bearer", "Br"),
      ("branch", "Brnch"),
      ("brand", "Brnd"),
      ("batch", "Btch"),
      ("box", "Bx"),
      ("card", "Card"),
      ("currency", "Ccy"),
      ("code", "Cd"),
      ("credit", "Cdt"),
      ("creditor", "Cdtr"),
      ("certificate", "Cert"),
      ("channel", "Chanl"),
      ("cheque", "Chq"),
      ("charge", "Chrg"),
      ("charges", "Chrgs"),
      ("city", "City"),
      ("clearing", "Clr"),
      ("component", "Cmpnt"),
      ("counter", "Cntr"),
      ("context", "Cntxt"),
      ("commission", "Comssn"),
      ("corporate", "Corp"),
      ("capability", "Cpbl"),
      ("capabilities", "Cpblties"),
      ("copy", "Cpy"),
      ("cardholder", "Crdhldr"),
      ("creation", "Cre"),
      ("cash", "Csh"),
      ("customer", "Cstmr"),
      ("contact", "Ctct"),
      ("category", "Ctgy"),
      ("contract", "Ctrct"),
      ("contractual", "Ctrctl"),
      ("country", "Ctry"),
      ("current", "Cur"),
      ("data", "Data"),
      ("days", "Days"),
      ("debit", "Dbt"),
      ("debtor", "Dbtr"),
      ("deal", "Deal"),
      ("decimal", "Dec"),
      ("department", "Dept"),
      ("description", "Desc"),
      ("display", "Disp"),
      ("delivering", "Dlvrg"),
      ("denomination", "Dnmtn"),
      ("document", "Doc"),
      ("domain", "Domn"),
      ("duplicate", "Dplct"),
      ("deposit", "Dpst"),
      ("discount", "Dscnt"),
      ("district", "Dstrct"),
      ("date", "Dt"),
      ("details", "Dtls"),
      ("dates", "Dts"),
      ("due", "Due"),
      ("division", "Dvsn"),
      ("electronic", "Elctrnc"),
      ("email", "Email"),
      ("end", "End"),
      ("envelope", "Envlp"),
      ("environment", "Envt"),
      ("event", "Evt"),
      ("face", "Face"),
      ("effective", "Fctv"),
      ("fax", "Fax"),
      ("forecast", "Fcst"),
      ("financial", "Fin"),
      ("fallback", "Fllbck"),
      ("floor", "Flr"),
      ("family", "Fmly"),
      ("from", "Fr"),
      ("forms", "Frms"),
      ("first", "Frst"),
      ("garnishee", "Grnshee"),
      ("garnishment", "Grnshmt"),
      ("group", "Grp"),
      ("header", "Hdr"),
      ("IBAN", "IBAN"),
      ("identification", "Id"),
      ("included", "Incl"),
      ("indicator", "Ind"),
      ("individual", "Indv"),
      ("information", "Inf"),
      ("infrastructure", "Infrstrctr"),
      ("initiating", "Initg"),
      ("input", "Inpt"),
      ("insurance", "Insrnc"),
      ("instructed", "Instd"),
      ("instructing", "Instg"),
      ("institution", "Instn"),
      ("instruction", "Instr"),
      ("instrument", "Instrm"),
      ("inter", "Intr"),
      ("intermediary", "Intrmy"),
      ("interest", "Intrst"),
      ("invoicee", "Invcee"),
      ("invoicer", "Invcr"),
      ("issuing", "Issg"),
      ("issuer", "Issr"),
      ("job", "Job"),
      ("language", "Lang"),
      ("last", "Last"),
      ("local", "Lcl"),
      ("location", "Lctn"),
      ("level", "Lvl"),
      ("legal", "Lgl"),
      ("LEI", "LEI"),
      ("line", "Line"),
      ("lines", "Lines"),
      ("manufacturer", "Manfctr"),
      ("mode", "Md"),
      ("medical", "Mdcl"),
      ("model", "Mdl"),
      ("measure", "Measr"),
      ("management", "Mgmt"),
      ("market", "Mkt"),
      ("member", "Mmb"),
      ("mandate", "Mndt"),
      ("mobile", "Mob"),
      ("employee", "Mplyee"),
      ("message", "Msg"),
      ("method", "Mtd"),
      ("number", "Nb"),
      ("net", "Net"),
      ("name", "Nm"),
      ("entries", "Ntries"),
      ("entry", "Ntry"),
      ("entity", "Ntty"),
      ("of", "Of"),
      ("or", "Or"),
      ("organisation", "Org"),
      ("original", "Orgnl"),
      ("originator", "Orgtr"),
      ("other", "Othr"),
      ("owner", "Ownr"),
      ("percentage", "Pctg"),
      ("paid", "Pd"),
      ("product", "Pdct"),
      ("page", "Pg"),
      ("POI", "POI"),
      ("CSC", "CSC"),
      ("pagination", "Pgntn"),
      ("phone", "Phne"),
      ("place", "Plc"),
      ("note", "Note"),
      ("and", "And"),
      ("unit", "Unit"),
      ("base", "Base"),
      ("birth", "Birth"),
      ("width", "Width"),
      ("BICFI", "BICFI"),
      ("equal", "EQ"),
      ("notEqual", "NEQ"),
      ("notes", "Notes"),
      ("payment", "Pmt"),
      ("any", "Any"),
      ("BIC", "BIC"),
      ("pre", "Pre"),
      ("ICC", "ICC"),
      ("re", "Re"),
      ("on", "On"),
      ("present", "Pres"),
      ("residence", "Res"),
      ("processing", "Prcg"),
      ("period", "Prd"),
      ("plain", "Plain"),
      ("preferred", "Prefrd"),
      ("presentment", "Presntmnt"),
      ("prefix", "Prfx"),
      ("price", "Pric"),
      ("print", "Prt"),
      ("PAN", "PAN"),
      ("ISIN", "ISIN"),
      ("zone", "Zone"),
      ("proprietary", "Prtry"),
      ("priority", "Prty"),
      ("per", "Per"),
      ("1", "1"),
      ("2", "2"),
      ("3", "3"),
      ("start", "Start"),
      ("UETR", "UETR"),
      ("province", "Prvc"),
      ("private", "Prvt"),
      ("proxy", "Prxy"),
      ("post", "Pst"),
      ("postal", "Pstl"),
      ("posting", "Pstng"),
      ("parties", "Pties"),
      ("party", "Pty"),
      ("purpose", "Purp"),
      ("payable", "Pybl"),
      ("query", "Qry"),
      ("quantities", "Qties"),
      ("quotation", "Qtn"),
      ("quantity", "Qty"),
      ("rate", "Rate"),
      ("reconciliation", "Rcncltn"),
      ("recipient", "Rcpt"),
      ("record", "Rcrd"),
      ("receiving", "Rcvg"),
      ("reading", "Rdng"),
      ("reference", "Ref"),
      ("references", "Refs"),
      ("registration", "Regn"),
      ("referred", "Rfrd"),
      ("range", "Rg"),
      ("related", "Rltd"),
      ("remittance", "Rmt"),
      ("remitted", "Rmtd"),
      ("room", "Room"),
      ("reporting", "Rptg"),
      ("reason", "Rsn"),
      ("responsibility", "Rspnsblty"),
      ("return", "Rtr"),
      ("reversal", "Rvsl"),
      ("sale", "Sale"),
      ("scheme", "Schme"),
      ("security", "Scty"),
      ("sequence", "Seq"),
      ("safekeeping", "Sfkpg"),
      ("suffix", "Sfx"),
      ("short", "Shrt"),
      ("supplementary", "Splmtry"),
      ("source", "Src"),
      ("serial", "Srl"),
      ("statement", "Stmt"),
      ("structured", "Strd"),
      ("structure", "Strt"),
      ("status", "Sts"),
      ("settlement", "Sttlm"),
      ("sub", "Sub"),
      ("sum", "Sum"),
      ("summary", "Summry"),
      ("service", "Svc"),
      ("servicer", "Svcr"),
      ("system", "Sys"),
      ("tax", "Tax"),
      ("taxable", "Taxbl"),
      ("technical", "Tech"),
      ("termination", "Termntn"),
      ("title", "Titl"),
      ("time", "Tm"),
      ("to", "To"),
      ("type", "Tp"),
      ("trade", "Trad"),
      ("trading", "Tradg"),
      ("track", "Trck"),
      ("target", "Trgt"),
      ("total", "Ttl"),
      ("town", "Twn"),
      ("transaction", "Tx"),
      ("transactions", "Txs"),
      ("ultimate", "Ultmt"),
      ("unstructured", "Ustrd"),
      ("value", "Val"),
      ("validation", "Vldtn"),
      ("validity", "Vldty"),
      ("verification", "Vrfctn"),
      ("version", "Vrsn"),
      ("waiver", "Wvr"),
      ("exchange", "Xchg"),
      ("expiry", "Xpry"),
      ("yielded", "Yldd"),
      ("year", "Yr")
    ]

instance MapNamesToXML (Camt53Element a) where
  mapNamesToElements l =
    (fromString $ mangleName l)
      { XML.nameNamespace = Just "urn:iso:std:iso:20022:tech:xsd:camt.053.001.11"
      }

  mapNamesToAttributes = fromString . mangleName

  -- Enums have their types prefixed to the constructors to disambiguate them. We
  -- can move them into their own modules to get rid of this.
  mapNamesToEnum s = case dropWhile (/= '_') s of
    '_' : rest -> rest
    _ -> s

mangleName :: String -> String
mangleName "" = ""
mangleName t =
  case mapMaybe (\i -> (length i,) <$> Map.lookup i unmanglingDictionary)
    . reverse
    $ inits t of
    (l, longestUnmangledMatch) : _ -> longestUnmangledMatch <> mangleName (drop l t)
    _ -> t

unmangleName :: String -> String
unmangleName s = go s
  where
    go :: String -> String
    go "" = []
    go t = case mapMaybe (\i -> (length i,) <$> Map.lookup i unmanglingDictionary) . reverse $ inits t of
      (l, longestUnmangledMatch) : _ -> longestUnmangledMatch <> capitalise (go $ drop l t)
      _ -> error $ "missing unmangle entry for " <> t <> " while unmangling " <> s
    capitalise :: String -> String
    capitalise = \case
      "" -> ""
      i : rest -> toUpper i : rest

unmangledElementName :: Text -> Name
unmangledElementName t =
  let uname = unmangleName $ Text.unpack t
   in if
        | uname == "type" -> mkName "elem_type"
        | i : _ <- uname, isUpper i -> mkName $ "elem_" <> uname
        | otherwise -> mkName uname

qCamt53Schema :: Q [Dec]
qCamt53Schema = do
  camt53xsd <- runIO $ Text.readFile "/home/frederikramcke/Downloads/camt.053.001.11.xsd"
  case decodeDocument camt53xsd of
    Left err -> error err
    Right SchemaDoc {schema} -> reifySchema schema

reifySchema :: Schema -> Q [Dec]
reifySchema s =
  let [documentRoot] = s ^. #element
   in pure $
        schema_document documentRoot
          : map reifySimpleType (s ^. #simpleType)
            <> map reifyComplexType (s ^. #complexType)

schema_document :: Element -> Dec
schema_document e =
  let typeName = mkName "DocumentRoot"
   in DataD [] typeName [] Nothing [RecC typeName [(mkName' $ "elem_" <> e ^. #attr_name, noBang, elementType e)]] []

reifySimpleType :: SimpleType -> Dec
reifySimpleType st =
  fromMaybe (error $ "unhandled SimpleType" <> show (ReadShowXmlElement st)) $
    simpleType_string_pattern st
      <|> simpleType_string_length st
      <|> simpleType_string_enumeration st
      <|> simpleType_boolean st
      <|> simpleType_decimal st
      <|> simpleType_date st
      <|> simpleType_time st
      <|> simpleType_year st
      <|> simpleType_month st

reifyComplexType :: ComplexType -> Dec
reifyComplexType st =
  fromMaybe (error "unhandled SimpleType") $
    complexType_sequence_elements st
      <|> complexType_sequence_any st
      <|> complexType_choice st
      <|> complexType_content_extension st

simpleType_string_pattern :: SimpleType -> Maybe Dec
simpleType_string_pattern st
  | r <- st ^. #restriction,
    r ^. #attr_base == "xs:string",
    Just (Value pattern) <- r ^. #pattern,
    r {pattern = Nothing} == emptyRestriction r =
      let nam = mkName' $ st ^. #attr_name
          accessorName = mkName' $ "un" <> st ^. #attr_name
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
              nam
              []
              Nothing
              (RecC nam [(accessorName, noBang, innerType)])
              [DerivClause (Just NewtypeStrategy) [ConT ''FromContent, ConT ''FromElement]]
  | otherwise = Nothing

-- todo: type alias instead of newtype?
simpleType_date :: SimpleType -> Maybe Dec
simpleType_date st
  | r <- st ^. #restriction,
    r ^. #attr_base == "xs:date",
    r == emptyRestriction r =
      let nam = mkName' $ st ^. #attr_name
          accessorName = mkName' $ "un" <> st ^. #attr_name
       in Just $
            NewtypeD
              []
              nam
              []
              Nothing
              (RecC nam [(accessorName, noBang, ConT ''Day)])
              [DerivClause (Just NewtypeStrategy) [ConT ''FromContent, ConT ''FromElement]]
  | otherwise = Nothing

-- just parse as text because what the hell even is this?
simpleType_year :: SimpleType -> Maybe Dec
simpleType_year st
  | r <- st ^. #restriction,
    r ^. #attr_base == "xs:gYear",
    r == emptyRestriction r =
      let nam = mkName' $ st ^. #attr_name
          accessorName = mkName' $ "un" <> st ^. #attr_name
       in Just $
            NewtypeD
              []
              nam
              []
              Nothing
              (RecC nam [(accessorName, noBang, ConT ''Text)])
              [DerivClause (Just NewtypeStrategy) [ConT ''FromContent, ConT ''FromElement]]
  | otherwise = Nothing

-- ditto
simpleType_month :: SimpleType -> Maybe Dec
simpleType_month st
  | r <- st ^. #restriction,
    r ^. #attr_base == "xs:gYearMonth",
    r == emptyRestriction r =
      let nam = mkName' $ st ^. #attr_name
          accessorName = mkName' $ "un" <> st ^. #attr_name
       in Just $
            NewtypeD
              []
              nam
              []
              Nothing
              (RecC nam [(accessorName, noBang, ConT ''Text)])
              [DerivClause (Just NewtypeStrategy) [ConT ''FromContent, ConT ''FromElement]]
  | otherwise = Nothing

simpleType_time :: SimpleType -> Maybe Dec
simpleType_time st
  | r <- st ^. #restriction,
    r ^. #attr_base == "xs:dateTime",
    r == emptyRestriction r =
      let nam = mkName' $ st ^. #attr_name
          accessorName = mkName' $ "un" <> st ^. #attr_name
       in Just $
            NewtypeD
              []
              nam
              []
              Nothing
              (RecC nam [(accessorName, noBang, ConT ''UTCTime)])
              [DerivClause (Just NewtypeStrategy) [ConT ''FromContent, ConT ''FromElement]]
  | otherwise = Nothing

simpleType_string_length :: SimpleType -> Maybe Dec
simpleType_string_length st
  | r <- st ^. #restriction,
    r ^. #attr_base == "xs:string",
    Just (Value l) <- r ^. #minLength,
    Just (Value u) <- r ^. #maxLength,
    r {minLength = Nothing, maxLength = Nothing} == emptyRestriction r =
      let nam = mkName' $ st ^. #attr_name
          accessorName = mkName' $ "un" <> st ^. #attr_name
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
              nam
              []
              Nothing
              (RecC nam [(accessorName, noBang, innerType)])
              [DerivClause (Just NewtypeStrategy) [ConT ''FromContent, ConT ''FromElement]]
  | otherwise = Nothing

-- derive MapNamesToXML via Camt53Names
-- derive FromContent via GenericEnum <typename>
-- derive FromElement via ContentElement <typename>
simpleType_string_enumeration :: SimpleType -> Maybe Dec
simpleType_string_enumeration st
  | r <- st ^. #restriction,
    r ^. #attr_base == "xs:string",
    not . null $ r ^. #enumeration,
    r {enumeration = []} == emptyRestriction r =
      let typeName = mkName' $ st ^. #attr_name
          conNames =
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
                DerivClause (Just $ ViaStrategy (AppT (ConT ''Camt53Enum) (ConT typeName))) [ConT ''FromContent, ConT ''FromElement]
              ]
  | otherwise = Nothing

simpleType_boolean :: SimpleType -> Maybe Dec
simpleType_boolean st
  | r <- st ^. #restriction,
    r ^. #attr_base == "xs:boolean",
    r == emptyRestriction r =
      let typeName = mkName' $ st ^. #attr_name
          accessorName = mkName' $ "un" <> st ^. #attr_name
       in Just $
            NewtypeD
              []
              typeName
              []
              Nothing
              (RecC typeName [(accessorName, noBang, ConT ''Bool)])
              [DerivClause (Just NewtypeStrategy) [ConT ''FromContent, ConT ''FromElement]]
  | otherwise = Nothing

-- todo: digit restrictions
simpleType_decimal :: SimpleType -> Maybe Dec
simpleType_decimal st
  | r <- st ^. #restriction,
    r ^. #attr_base == "xs:decimal",
    r {fractionDigits = Nothing, totalDigits = Nothing, minInclusive = Nothing}
      == emptyRestriction r =
      let typeName = mkName' $ st ^. #attr_name
          accessorName = mkName' $ "un" <> st ^. #attr_name
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
              [DerivClause (Just NewtypeStrategy) [ConT ''FromContent, ConT ''FromElement]]
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

complexType_sequence_elements :: ComplexType -> Maybe Dec
complexType_sequence_elements ct
  | ComplexTypeSequence {sequence = s} <- ct,
    isNothing $ s ^. #any =
      let mkField el = (unmangledElementName $ el ^. #attr_name, noBang, elementType el)
          typeName = mkName' $ ct ^. #attr_name
       in Just $
            DataD
              []
              typeName
              []
              Nothing
              [RecC typeName (map mkField $ s ^. #element)]
              [ DerivClause (Just StockStrategy) [ConT ''Generic],
                DerivClause (Just $ ViaStrategy (AppT (ConT ''Camt53Element) (ConT typeName))) [ConT ''FromElement]
              ]
  | otherwise = Nothing

-- todo
complexType_sequence_any :: ComplexType -> Maybe Dec
complexType_sequence_any ct
  | ComplexTypeSequence {sequence = s} <- ct,
    null $ s ^. #element,
    isJust $ s ^. #any =
      let typeName = mkName' $ ct ^. #attr_name
       in Just $
            DataD
              []
              typeName
              []
              Nothing
              [RecC typeName [(mkName "any", noBang, ConT ''Leftovers)]]
              [ DerivClause (Just StockStrategy) [ConT ''Generic],
                DerivClause (Just $ ViaStrategy (AppT (ConT ''Camt53Element) (ConT typeName))) [ConT ''FromElement]
              ]
  | otherwise = Nothing

complexType_choice :: ComplexType -> Maybe Dec
complexType_choice ct
  | ComplexTypeChoice {choice = Choice {element = e}} <- ct =
      let mkConstructor :: Element -> Con
          mkConstructor element' =
            RecC
              (mkName' $ ct ^. #attr_name <> element' ^. #attr_name)
              [(unmangledElementName $ element' ^. #attr_name, noBang, elementType element')]
          typeName = mkName' $ ct ^. #attr_name
       in Just $
            DataD
              []
              typeName
              []
              Nothing
              (map mkConstructor e)
              [ DerivClause (Just StockStrategy) [ConT ''Generic],
                DerivClause (Just $ ViaStrategy (AppT (ConT ''Camt53Element) (ConT typeName))) [ConT ''FromElement]
              ]
  | otherwise = Nothing

complexType_content_extension :: ComplexType -> Maybe Dec
complexType_content_extension ct
  | ComplexTypeContent {simpleContent = SimpleContent {extension = e}} <- ct,
    e ^. #attribute % #attr_use == Required =
      let attributeField =
            ( mkName $ "attr_" <> unmangleName (Text.unpack $ e ^. #attribute % #attr_name),
              noBang,
              ConT . mkName' $ e ^. #attribute % #attr_type
            )
          contentField =
            ( mkName "content",
              noBang,
              ConT $ mkName' $ e ^. #attr_base
            )
          typeName = mkName' $ ct ^. #attr_name
       in Just $
            DataD
              []
              typeName
              []
              Nothing
              [RecC typeName [attributeField, contentField]]
              [ DerivClause (Just StockStrategy) [ConT ''Generic],
                DerivClause (Just $ ViaStrategy (AppT (ConT ''Camt53Content) (ConT typeName))) [ConT ''FromElement]
              ]
  | otherwise = Nothing

newtype Camt53Element a = Camt53Element a
  deriving newtype (Generic)

instance
  (FromElement (GenericOrdered (Camt53Element a))) =>
  FromElement (Camt53Element a)
  where
  fromElement el = (\(GenericOrdered a) -> a) <$> fromElement el

newtype Camt53Enum a = Camt53Enum a
  deriving (MapNamesToXML) via Camt53Element a
  deriving newtype (Generic, FromElement)

instance
  (FromContent (GenericEnum (Camt53Enum a))) =>
  FromContent (Camt53Enum a)
  where
  fromContent t i = (\(GenericEnum a) -> a) <$> fromContent t i

newtype Camt53Content a = Camt53Content a
  deriving (MapNamesToXML) via Camt53Element a
  deriving newtype (Generic)

instance
  (FromElement (GenericContent (Camt53Content a))) =>
  FromElement (Camt53Content a)
  where
  fromElement el = (\(GenericContent a) -> a) <$> fromElement el

{-
todo:
- toElement, test round-trip
- Decimal type
-}
