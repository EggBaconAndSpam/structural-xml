{-# LANGUAGE TupleSections #-}

module Data.XML.ISO20022.NameMangling where

import Data.Char
import Data.List (inits, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Tuple

mangleName :: String -> String
mangleName "" = ""
mangleName t =
  case mapMaybe (\i -> (length i,) <$> Map.lookup i manglingDictionary)
    . sortOn ((0 -) . length)
    $ inits t <> inits (uncapitalise t) of
    (l, longestUnmangledMatch) : _ -> longestUnmangledMatch <> mangleName (drop l t)
    _ -> t
  where
    uncapitalise :: String -> String
    uncapitalise = \case
      "" -> ""
      i : rest -> toLower i : rest

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

unmanglingDictionary :: Map String String
unmanglingDictionary = Map.fromList . map swap $ Map.toList manglingDictionary

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
      ("year", "Yr"),
      -- CAMT053.02
      ("BEI", "BEI"),
      ("zone", "Zn")
    ]