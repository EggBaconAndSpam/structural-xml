{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS -ddump-splices -ddump-to-file #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test where

import Data.Decimal
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Time
import Data.XML hiding (Document)
import Data.XML.Camt53
import Data.XML.ISO20022.Restricted
import GHC.Generics

$(qCamt53Schema)

{-
data DocumentRoot
  = DocumentRoot {elem_Document :: Document}
  deriving stock (Generic)
  deriving (FromDocument, ToDocument) via Camt53Document DocumentRoot

newtype ActiveOrHistoricCurrencyAndAmount_SimpleType
  = ActiveOrHistoricCurrencyAndAmount_SimpleType
  { unActiveOrHistoricCurrencyAndAmount_SimpleType :: Restricted '[ 'FractionDigits 5, 'MinInclusive 0, 'TotalDigits 18] Decimal
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype ActiveOrHistoricCurrencyCode
  = ActiveOrHistoricCurrencyCode {unActiveOrHistoricCurrencyCode :: Restricted '[ 'Pattern "[A-Z]{3,3}"] Text}
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

data AddressType2Code
  = AddressType2Code_ADDR
  | AddressType2Code_PBOX
  | AddressType2Code_HOME
  | AddressType2Code_BIZZ
  | AddressType2Code_MLTO
  | AddressType2Code_DLVY
  deriving stock (Generic)
  deriving (FromContent, FromElement, ToContent, ToElement) via Camt53Enum AddressType2Code

newtype AnyBICIdentifier
  = AnyBICIdentifier {unAnyBICIdentifier :: Restricted '[ 'Pattern "[A-Z]{6,6}[A-Z2-9][A-NP-Z0-9]([A-Z0-9]{3,3}){0,1}"] Text}
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype BICIdentifier
  = BICIdentifier {unBICIdentifier :: Restricted '[ 'Pattern "[A-Z]{6,6}[A-Z2-9][A-NP-Z0-9]([A-Z0-9]{3,3}){0,1}"] Text}
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

data BalanceType12Code
  = BalanceType12Code_XPCD
  | BalanceType12Code_OPAV
  | BalanceType12Code_ITAV
  | BalanceType12Code_CLAV
  | BalanceType12Code_FWAV
  | BalanceType12Code_CLBD
  | BalanceType12Code_ITBD
  | BalanceType12Code_OPBD
  | BalanceType12Code_PRCD
  | BalanceType12Code_INFO
  deriving stock (Generic)
  deriving (FromContent, FromElement, ToContent, ToElement) via Camt53Enum BalanceType12Code

newtype BaseOneRate
  = BaseOneRate
  { unBaseOneRate ::
      Restricted
        '[ 'FractionDigits 10,
           'TotalDigits 11
         ]
        Decimal
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

data CashAccountType4Code
  = CashAccountType4Code_CASH
  | CashAccountType4Code_CHAR
  | CashAccountType4Code_COMM
  | CashAccountType4Code_TAXE
  | CashAccountType4Code_CISH
  | CashAccountType4Code_TRAS
  | CashAccountType4Code_SACC
  | CashAccountType4Code_CACC
  | CashAccountType4Code_SVGS
  | CashAccountType4Code_ONDP
  | CashAccountType4Code_MGLD
  | CashAccountType4Code_NREX
  | CashAccountType4Code_MOMA
  | CashAccountType4Code_LOAN
  | CashAccountType4Code_SLRY
  | CashAccountType4Code_ODFT
  deriving stock (Generic)
  deriving (FromContent, FromElement, ToContent, ToElement) via Camt53Enum CashAccountType4Code

data ChargeBearerType1Code
  = ChargeBearerType1Code_DEBT
  | ChargeBearerType1Code_CRED
  | ChargeBearerType1Code_SHAR
  | ChargeBearerType1Code_SLEV
  deriving stock (Generic)
  deriving (FromContent, FromElement, ToContent, ToElement) via Camt53Enum ChargeBearerType1Code

data ChargeType1Code
  = ChargeType1Code_BRKF
  | ChargeType1Code_COMM
  deriving stock (Generic)
  deriving (FromContent, FromElement, ToContent, ToElement) via Camt53Enum ChargeType1Code

data CopyDuplicate1Code
  = CopyDuplicate1Code_CODU
  | CopyDuplicate1Code_COPY
  | CopyDuplicate1Code_DUPL
  deriving stock (Generic)
  deriving (FromContent, FromElement, ToContent, ToElement) via Camt53Enum CopyDuplicate1Code

newtype CountryCode
  = CountryCode {unCountryCode :: Restricted '[ 'Pattern "[A-Z]{2,2}"] Text}
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

data CreditDebitCode
  = CreditDebitCode_CRDT
  | CreditDebitCode_DBIT
  deriving stock (Generic)
  deriving (FromContent, FromElement, ToContent, ToElement) via Camt53Enum CreditDebitCode

newtype DecimalNumber
  = DecimalNumber
  { unDecimalNumber :: Restricted '[ 'FractionDigits 17, 'TotalDigits 18] Decimal
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

data DocumentType3Code
  = DocumentType3Code_RADM
  | DocumentType3Code_RPIN
  | DocumentType3Code_FXDR
  | DocumentType3Code_DISP
  | DocumentType3Code_PUOR
  | DocumentType3Code_SCOR
  deriving stock (Generic)
  deriving (FromContent, FromElement, ToContent, ToElement) via Camt53Enum DocumentType3Code

data DocumentType5Code
  = DocumentType5Code_MSIN
  | DocumentType5Code_CNFA
  | DocumentType5Code_DNFA
  | DocumentType5Code_CINV
  | DocumentType5Code_CREN
  | DocumentType5Code_DEBN
  | DocumentType5Code_HIRI
  | DocumentType5Code_SBIN
  | DocumentType5Code_CMCN
  | DocumentType5Code_SOAC
  | DocumentType5Code_DISP
  | DocumentType5Code_BOLD
  | DocumentType5Code_VCHR
  | DocumentType5Code_AROI
  | DocumentType5Code_TSUT
  deriving stock (Generic)
  deriving (FromContent, FromElement, ToContent, ToElement) via Camt53Enum DocumentType5Code

data EntryStatus2Code
  = EntryStatus2Code_BOOK
  | EntryStatus2Code_PDNG
  | EntryStatus2Code_INFO
  deriving stock (Generic)
  deriving (FromContent, FromElement, ToContent, ToElement) via Camt53Enum EntryStatus2Code

newtype ExternalAccountIdentification1Code
  = ExternalAccountIdentification1Code
  { unExternalAccountIdentification1Code :: Restricted '[ 'MinLength 1, 'MaxLength 4] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype ExternalBalanceSubType1Code
  = ExternalBalanceSubType1Code
  { unExternalBalanceSubType1Code :: Restricted '[ 'MinLength 1, 'MaxLength 4] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype ExternalBankTransactionDomain1Code
  = ExternalBankTransactionDomain1Code
  { unExternalBankTransactionDomain1Code :: Restricted '[ 'MinLength 1, 'MaxLength 4] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype ExternalBankTransactionFamily1Code
  = ExternalBankTransactionFamily1Code
  { unExternalBankTransactionFamily1Code :: Restricted '[ 'MinLength 1, 'MaxLength 4] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype ExternalBankTransactionSubFamily1Code
  = ExternalBankTransactionSubFamily1Code
  { unExternalBankTransactionSubFamily1Code :: Restricted '[ 'MinLength 1, 'MaxLength 4] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype ExternalClearingSystemIdentification1Code
  = ExternalClearingSystemIdentification1Code
  { unExternalClearingSystemIdentification1Code :: Restricted '[ 'MinLength 1, 'MaxLength 5] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype ExternalFinancialInstitutionIdentification1Code
  = ExternalFinancialInstitutionIdentification1Code
  { unExternalFinancialInstitutionIdentification1Code :: Restricted '[ 'MinLength 1, 'MaxLength 4] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype ExternalOrganisationIdentification1Code
  = ExternalOrganisationIdentification1Code
  { unExternalOrganisationIdentification1Code :: Restricted '[ 'MinLength 1, 'MaxLength 4] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype ExternalPersonIdentification1Code
  = ExternalPersonIdentification1Code
  { unExternalPersonIdentification1Code :: Restricted '[ 'MinLength 1, 'MaxLength 4] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype ExternalPurpose1Code
  = ExternalPurpose1Code
  { unExternalPurpose1Code :: Restricted '[ 'MinLength 1, 'MaxLength 4] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype ExternalReportingSource1Code
  = ExternalReportingSource1Code
  { unExternalReportingSource1Code :: Restricted '[ 'MinLength 1, 'MaxLength 4] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype ExternalReturnReason1Code
  = ExternalReturnReason1Code
  { unExternalReturnReason1Code :: Restricted '[ 'MinLength 1, 'MaxLength 4] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype ExternalTechnicalInputChannel1Code
  = ExternalTechnicalInputChannel1Code
  { unExternalTechnicalInputChannel1Code :: Restricted '[ 'MinLength 1, 'MaxLength 4] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype IBAN2007Identifier
  = IBAN2007Identifier {unIBAN2007Identifier :: Restricted '[ 'Pattern "[A-Z]{2,2}[0-9]{2,2}[a-zA-Z0-9]{1,30}"] Text}
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype ISINIdentifier
  = ISINIdentifier {unISINIdentifier :: Restricted '[ 'Pattern "[A-Z0-9]{12,12}"] Text}
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype ISODate
  = ISODate {unISODate :: Day}
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype ISODateTime
  = ISODateTime {unISODateTime :: UTCTime}
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype ImpliedCurrencyAndAmount
  = ImpliedCurrencyAndAmount
  { unImpliedCurrencyAndAmount :: Restricted '[ 'FractionDigits 5, 'MinInclusive 0, 'TotalDigits 18] Decimal
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

data InterestType1Code
  = InterestType1Code_INDY
  | InterestType1Code_OVRN
  deriving stock (Generic)
  deriving (FromContent, FromElement, ToContent, ToElement) via Camt53Enum InterestType1Code

newtype Max105Text
  = Max105Text
  { unMax105Text :: Restricted '[ 'MinLength 1, 'MaxLength 105] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype Max140Text
  = Max140Text
  { unMax140Text :: Restricted '[ 'MinLength 1, 'MaxLength 140] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype Max15NumericText
  = Max15NumericText {unMax15NumericText :: Restricted '[ 'Pattern "[0-9]{1,15}"] Text}
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype Max15PlusSignedNumericText
  = Max15PlusSignedNumericText {unMax15PlusSignedNumericText :: Restricted '[ 'Pattern "[+]{0,1}[0-9]{1,15}"] Text}
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype Max16Text
  = Max16Text
  { unMax16Text :: Restricted '[ 'MinLength 1, 'MaxLength 16] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype Max2048Text
  = Max2048Text
  { unMax2048Text :: Restricted '[ 'MinLength 1, 'MaxLength 2048] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype Max34Text
  = Max34Text
  { unMax34Text :: Restricted '[ 'MinLength 1, 'MaxLength 34] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype Max35Text
  = Max35Text
  { unMax35Text :: Restricted '[ 'MinLength 1, 'MaxLength 35] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype Max4Text
  = Max4Text
  { unMax4Text :: Restricted '[ 'MinLength 1, 'MaxLength 4] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype Max500Text
  = Max500Text
  { unMax500Text :: Restricted '[ 'MinLength 1, 'MaxLength 500] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype Max5NumericText
  = Max5NumericText {unMax5NumericText :: Restricted '[ 'Pattern "[0-9]{1,5}"] Text}
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype Max70Text
  = Max70Text
  { unMax70Text :: Restricted '[ 'MinLength 1, 'MaxLength 70] Text
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

data NamePrefix1Code
  = NamePrefix1Code_DOCT
  | NamePrefix1Code_MIST
  | NamePrefix1Code_MISS
  | NamePrefix1Code_MADM
  deriving stock (Generic)
  deriving (FromContent, FromElement, ToContent, ToElement) via Camt53Enum NamePrefix1Code

newtype Number
  = Number
  { unNumber :: Restricted '[ 'FractionDigits 0, 'TotalDigits 18] Decimal
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype PercentageRate
  = PercentageRate
  { unPercentageRate :: Restricted '[ 'FractionDigits 10, 'TotalDigits 11] Decimal
  }
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype PhoneNumber
  = PhoneNumber {unPhoneNumber :: Restricted '[ 'Pattern "\\+[0-9]{1,3}-[0-9()+\\-]{1,30}"] Text}
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

data RemittanceLocationMethod2Code
  = RemittanceLocationMethod2Code_FAXI
  | RemittanceLocationMethod2Code_EDIC
  | RemittanceLocationMethod2Code_URID
  | RemittanceLocationMethod2Code_EMAL
  | RemittanceLocationMethod2Code_POST
  | RemittanceLocationMethod2Code_SMSM
  deriving stock (Generic)
  deriving (FromContent, FromElement, ToContent, ToElement) via Camt53Enum RemittanceLocationMethod2Code

data TaxRecordPeriod1Code
  = TaxRecordPeriod1Code_MM01
  | TaxRecordPeriod1Code_MM02
  | TaxRecordPeriod1Code_MM03
  | TaxRecordPeriod1Code_MM04
  | TaxRecordPeriod1Code_MM05
  | TaxRecordPeriod1Code_MM06
  | TaxRecordPeriod1Code_MM07
  | TaxRecordPeriod1Code_MM08
  | TaxRecordPeriod1Code_MM09
  | TaxRecordPeriod1Code_MM10
  | TaxRecordPeriod1Code_MM11
  | TaxRecordPeriod1Code_MM12
  | TaxRecordPeriod1Code_QTR1
  | TaxRecordPeriod1Code_QTR2
  | TaxRecordPeriod1Code_QTR3
  | TaxRecordPeriod1Code_QTR4
  | TaxRecordPeriod1Code_HLF1
  | TaxRecordPeriod1Code_HLF2
  deriving stock (Generic)
  deriving (FromContent, FromElement, ToContent, ToElement) via Camt53Enum TaxRecordPeriod1Code

newtype TrueFalseIndicator
  = TrueFalseIndicator {unTrueFalseIndicator :: Bool}
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

newtype YesNoIndicator
  = YesNoIndicator {unYesNoIndicator :: Bool}
  deriving newtype (FromContent, FromElement, ToContent, ToElement)

data AccountIdentification4Choice
  = AccountIdentification4ChoiceIBAN {elem_IBAN :: IBAN2007Identifier}
  | AccountIdentification4ChoiceOthr {other :: GenericAccountIdentification1}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element AccountIdentification4Choice

data AccountInterest2
  = AccountInterest2
  { elem_type :: Maybe InterestType1Choice,
    rate :: [Rate3],
    fromToDate :: Maybe DateTimePeriodDetails,
    reason :: Maybe Max35Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element AccountInterest2

data AccountSchemeName1Choice
  = AccountSchemeName1ChoiceCd {code :: ExternalAccountIdentification1Code}
  | AccountSchemeName1ChoicePrtry {proprietary :: Max35Text}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element AccountSchemeName1Choice

data AccountStatement2
  = AccountStatement2
  { identification :: Max35Text,
    electronicSequenceNumber :: Maybe Number,
    legalSequenceNumber :: Maybe Number,
    creationDateTime :: ISODateTime,
    fromToDate :: Maybe DateTimePeriodDetails,
    copyDuplicateIndicator :: Maybe CopyDuplicate1Code,
    reportingSource :: Maybe ReportingSource1Choice,
    account :: CashAccount20,
    relatedAccount :: Maybe CashAccount16,
    interest :: [AccountInterest2],
    balance :: NonEmpty CashBalance3,
    transactionsSummary :: Maybe TotalTransactions2,
    entry :: [ReportEntry2],
    additionalStatementInformation :: Maybe Max500Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element AccountStatement2

data ActiveOrHistoricCurrencyAndAmount
  = ActiveOrHistoricCurrencyAndAmount
  { attr_currency :: ActiveOrHistoricCurrencyCode,
    content :: ActiveOrHistoricCurrencyAndAmount_SimpleType
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Content ActiveOrHistoricCurrencyAndAmount

data AlternateSecurityIdentification2
  = AlternateSecurityIdentification2
  { elem_type :: Max35Text,
    identification :: Max35Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element AlternateSecurityIdentification2

data AmountAndCurrencyExchange3
  = AmountAndCurrencyExchange3
  { instructedAmount :: Maybe AmountAndCurrencyExchangeDetails3,
    transactionAmount :: Maybe AmountAndCurrencyExchangeDetails3,
    counterValueAmount :: Maybe AmountAndCurrencyExchangeDetails3,
    announcedPostingAmount :: Maybe AmountAndCurrencyExchangeDetails3,
    proprietaryAmount :: [AmountAndCurrencyExchangeDetails4]
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element AmountAndCurrencyExchange3

data AmountAndCurrencyExchangeDetails3
  = AmountAndCurrencyExchangeDetails3
  { amount :: ActiveOrHistoricCurrencyAndAmount,
    currencyExchange :: Maybe CurrencyExchange5
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element AmountAndCurrencyExchangeDetails3

data AmountAndCurrencyExchangeDetails4
  = AmountAndCurrencyExchangeDetails4
  { elem_type :: Max35Text,
    amount :: ActiveOrHistoricCurrencyAndAmount,
    currencyExchange :: Maybe CurrencyExchange5
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element AmountAndCurrencyExchangeDetails4

data AmountRangeBoundary1
  = AmountRangeBoundary1
  { boundaryAmount :: ImpliedCurrencyAndAmount,
    included :: YesNoIndicator
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element AmountRangeBoundary1

data BalanceSubType1Choice
  = BalanceSubType1ChoiceCd {code :: ExternalBalanceSubType1Code}
  | BalanceSubType1ChoicePrtry {proprietary :: Max35Text}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element BalanceSubType1Choice

data BalanceType12
  = BalanceType12
  { codeOrProprietary :: BalanceType5Choice,
    subType :: Maybe BalanceSubType1Choice
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element BalanceType12

data BalanceType5Choice
  = BalanceType5ChoiceCd {code :: BalanceType12Code}
  | BalanceType5ChoicePrtry {proprietary :: Max35Text}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element BalanceType5Choice

data BankToCustomerStatementV02
  = BankToCustomerStatementV02
  { groupHeader :: GroupHeader42,
    statement :: NonEmpty AccountStatement2
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element BankToCustomerStatementV02

data BankTransactionCodeStructure4
  = BankTransactionCodeStructure4
  { domain :: Maybe BankTransactionCodeStructure5,
    proprietary :: Maybe ProprietaryBankTransactionCodeStructure1
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element BankTransactionCodeStructure4

data BankTransactionCodeStructure5
  = BankTransactionCodeStructure5
  { code :: ExternalBankTransactionDomain1Code,
    family :: BankTransactionCodeStructure6
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element BankTransactionCodeStructure5

data BankTransactionCodeStructure6
  = BankTransactionCodeStructure6
  { code :: ExternalBankTransactionFamily1Code,
    subFamilyCode :: ExternalBankTransactionSubFamily1Code
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element BankTransactionCodeStructure6

data BatchInformation2
  = BatchInformation2
  { messageIdentification :: Maybe Max35Text,
    paymentInformationIdentification :: Maybe Max35Text,
    numberOfTransactions :: Maybe Max15NumericText,
    totalAmount :: Maybe ActiveOrHistoricCurrencyAndAmount,
    creditDebitIndicator :: Maybe CreditDebitCode
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element BatchInformation2

data BranchAndFinancialInstitutionIdentification4
  = BranchAndFinancialInstitutionIdentification4
  { financialInstitutionIdentification :: FinancialInstitutionIdentification7,
    branchIdentification :: Maybe BranchData2
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element BranchAndFinancialInstitutionIdentification4

data BranchData2
  = BranchData2
  { identification :: Maybe Max35Text,
    name :: Maybe Max140Text,
    postalAddress :: Maybe PostalAddress6
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element BranchData2

data CashAccount16
  = CashAccount16
  { identification :: AccountIdentification4Choice,
    elem_type :: Maybe CashAccountType2,
    currency :: Maybe ActiveOrHistoricCurrencyCode,
    name :: Maybe Max70Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element CashAccount16

data CashAccount20
  = CashAccount20
  { identification :: AccountIdentification4Choice,
    elem_type :: Maybe CashAccountType2,
    currency :: Maybe ActiveOrHistoricCurrencyCode,
    name :: Maybe Max70Text,
    owner :: Maybe PartyIdentification32,
    servicer :: Maybe BranchAndFinancialInstitutionIdentification4
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element CashAccount20

data CashAccountType2
  = CashAccountType2Cd {code :: CashAccountType4Code}
  | CashAccountType2Prtry {proprietary :: Max35Text}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element CashAccountType2

data CashBalance3
  = CashBalance3
  { elem_type :: BalanceType12,
    creditLine :: Maybe CreditLine2,
    amount :: ActiveOrHistoricCurrencyAndAmount,
    creditDebitIndicator :: CreditDebitCode,
    date :: DateAndDateTimeChoice,
    availability :: [CashBalanceAvailability2]
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element CashBalance3

data CashBalanceAvailability2
  = CashBalanceAvailability2
  { date :: CashBalanceAvailabilityDate1,
    amount :: ActiveOrHistoricCurrencyAndAmount,
    creditDebitIndicator :: CreditDebitCode
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element CashBalanceAvailability2

data CashBalanceAvailabilityDate1
  = CashBalanceAvailabilityDate1NbOfDays {numberOfDays :: Max15PlusSignedNumericText}
  | CashBalanceAvailabilityDate1ActlDt {actualDate :: ISODate}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element CashBalanceAvailabilityDate1

data ChargeType2Choice
  = ChargeType2ChoiceCd {code :: ChargeType1Code}
  | ChargeType2ChoicePrtry {proprietary :: GenericIdentification3}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element ChargeType2Choice

data ChargesInformation6
  = ChargesInformation6
  { totalChargesAndTaxAmount :: Maybe ActiveOrHistoricCurrencyAndAmount,
    amount :: ActiveOrHistoricCurrencyAndAmount,
    creditDebitIndicator :: Maybe CreditDebitCode,
    elem_type :: Maybe ChargeType2Choice,
    rate :: Maybe PercentageRate,
    bearer :: Maybe ChargeBearerType1Code,
    party :: Maybe BranchAndFinancialInstitutionIdentification4,
    tax :: Maybe TaxCharges2
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element ChargesInformation6

data ClearingSystemIdentification2Choice
  = ClearingSystemIdentification2ChoiceCd {code :: ExternalClearingSystemIdentification1Code}
  | ClearingSystemIdentification2ChoicePrtry {proprietary :: Max35Text}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element ClearingSystemIdentification2Choice

data ClearingSystemMemberIdentification2
  = ClearingSystemMemberIdentification2
  { clearingSystemIdentification :: Maybe ClearingSystemIdentification2Choice,
    memberIdentification :: Max35Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element ClearingSystemMemberIdentification2

data ContactDetails2
  = ContactDetails2
  { namePrefix :: Maybe NamePrefix1Code,
    name :: Maybe Max140Text,
    phoneNumber :: Maybe PhoneNumber,
    mobileNumber :: Maybe PhoneNumber,
    faxNumber :: Maybe PhoneNumber,
    emailAddress :: Maybe Max2048Text,
    other :: Maybe Max35Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element ContactDetails2

data CorporateAction1
  = CorporateAction1
  { code :: Maybe Max35Text,
    number :: Maybe Max35Text,
    proprietary :: Maybe Max35Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element CorporateAction1

data CreditLine2
  = CreditLine2
  { included :: TrueFalseIndicator,
    amount :: Maybe ActiveOrHistoricCurrencyAndAmount
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element CreditLine2

data CreditorReferenceInformation2
  = CreditorReferenceInformation2
  { elem_type :: Maybe CreditorReferenceType2,
    reference :: Maybe Max35Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element CreditorReferenceInformation2

data CreditorReferenceType1Choice
  = CreditorReferenceType1ChoiceCd {code :: DocumentType3Code}
  | CreditorReferenceType1ChoicePrtry {proprietary :: Max35Text}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element CreditorReferenceType1Choice

data CreditorReferenceType2
  = CreditorReferenceType2
  { codeOrProprietary :: CreditorReferenceType1Choice,
    issuer :: Maybe Max35Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element CreditorReferenceType2

data CurrencyAndAmountRange2
  = CurrencyAndAmountRange2
  { amount :: ImpliedCurrencyAmountRangeChoice,
    creditDebitIndicator :: Maybe CreditDebitCode,
    currency :: ActiveOrHistoricCurrencyCode
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element CurrencyAndAmountRange2

data CurrencyExchange5
  = CurrencyExchange5
  { sourceCurrency :: ActiveOrHistoricCurrencyCode,
    targetCurrency :: Maybe ActiveOrHistoricCurrencyCode,
    unitCurrency :: Maybe ActiveOrHistoricCurrencyCode,
    exchangeRate :: BaseOneRate,
    contractIdentification :: Maybe Max35Text,
    quotationDate :: Maybe ISODateTime
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element CurrencyExchange5

data DateAndDateTimeChoice
  = DateAndDateTimeChoiceDt {date :: ISODate}
  | DateAndDateTimeChoiceDtTm {dateTime :: ISODateTime}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element DateAndDateTimeChoice

data DateAndPlaceOfBirth
  = DateAndPlaceOfBirth
  { birthDate :: ISODate,
    provinceOfBirth :: Maybe Max35Text,
    cityOfBirth :: Max35Text,
    countryOfBirth :: CountryCode
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element DateAndPlaceOfBirth

data DatePeriodDetails
  = DatePeriodDetails {fromDate :: ISODate, toDate :: ISODate}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element DatePeriodDetails

data DateTimePeriodDetails
  = DateTimePeriodDetails
  { fromDateTime :: ISODateTime,
    toDateTime :: ISODateTime
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element DateTimePeriodDetails

data Document
  = Document {bankToCustomerStatement :: BankToCustomerStatementV02}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element Document

data DocumentAdjustment1
  = DocumentAdjustment1
  { amount :: ActiveOrHistoricCurrencyAndAmount,
    creditDebitIndicator :: Maybe CreditDebitCode,
    reason :: Maybe Max4Text,
    additionalInformation :: Maybe Max140Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element DocumentAdjustment1

data EntryDetails1
  = EntryDetails1
  { batch :: Maybe BatchInformation2,
    transactionDetails :: [EntryTransaction2]
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element EntryDetails1

data EntryTransaction2
  = EntryTransaction2
  { references :: Maybe TransactionReferences2,
    amountDetails :: Maybe AmountAndCurrencyExchange3,
    availability :: [CashBalanceAvailability2],
    bankTransactionCode :: Maybe BankTransactionCodeStructure4,
    charges :: [ChargesInformation6],
    interest :: [TransactionInterest2],
    relatedParties :: Maybe TransactionParty2,
    relatedAgents :: Maybe TransactionAgents2,
    purpose :: Maybe Purpose2Choice,
    relatedRemittanceInformation :: Restricted '[ 'MaxLength 10] [RemittanceLocation2],
    remittanceInformation :: Maybe RemittanceInformation5,
    relatedDates :: Maybe TransactionDates2,
    relatedPrice :: Maybe TransactionPrice2Choice,
    relatedQuantities :: [TransactionQuantities1Choice],
    financialInstrumentIdentification :: Maybe SecurityIdentification4Choice,
    tax :: Maybe TaxInformation3,
    returnInformation :: Maybe ReturnReasonInformation10,
    corporateAction :: Maybe CorporateAction1,
    safekeepingAccount :: Maybe CashAccount16,
    additionalTransactionInformation :: Maybe Max500Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element EntryTransaction2

data FinancialIdentificationSchemeName1Choice
  = FinancialIdentificationSchemeName1ChoiceCd {code :: ExternalFinancialInstitutionIdentification1Code}
  | FinancialIdentificationSchemeName1ChoicePrtry {proprietary :: Max35Text}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element FinancialIdentificationSchemeName1Choice

data FinancialInstitutionIdentification7
  = FinancialInstitutionIdentification7
  { elem_BIC :: Maybe BICIdentifier,
    clearingSystemMemberIdentification :: Maybe ClearingSystemMemberIdentification2,
    name :: Maybe Max140Text,
    postalAddress :: Maybe PostalAddress6,
    other :: Maybe GenericFinancialIdentification1
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element FinancialInstitutionIdentification7

data FinancialInstrumentQuantityChoice
  = FinancialInstrumentQuantityChoiceUnit {unit :: DecimalNumber}
  | FinancialInstrumentQuantityChoiceFaceAmt {faceAmount :: ImpliedCurrencyAndAmount}
  | FinancialInstrumentQuantityChoiceAmtsdVal {amortisedValue :: ImpliedCurrencyAndAmount}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element FinancialInstrumentQuantityChoice

data FromToAmountRange
  = FromToAmountRange
  { fromAmount :: AmountRangeBoundary1,
    toAmount :: AmountRangeBoundary1
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element FromToAmountRange

data GenericAccountIdentification1
  = GenericAccountIdentification1
  { identification :: Max34Text,
    schemeName :: Maybe AccountSchemeName1Choice,
    issuer :: Maybe Max35Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element GenericAccountIdentification1

data GenericFinancialIdentification1
  = GenericFinancialIdentification1
  { identification :: Max35Text,
    schemeName :: Maybe FinancialIdentificationSchemeName1Choice,
    issuer :: Maybe Max35Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element GenericFinancialIdentification1

data GenericIdentification3
  = GenericIdentification3
  { identification :: Max35Text,
    issuer :: Maybe Max35Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element GenericIdentification3

data GenericOrganisationIdentification1
  = GenericOrganisationIdentification1
  { identification :: Max35Text,
    schemeName :: Maybe OrganisationIdentificationSchemeName1Choice,
    issuer :: Maybe Max35Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element GenericOrganisationIdentification1

data GenericPersonIdentification1
  = GenericPersonIdentification1
  { identification :: Max35Text,
    schemeName :: Maybe PersonIdentificationSchemeName1Choice,
    issuer :: Maybe Max35Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element GenericPersonIdentification1

data GroupHeader42
  = GroupHeader42
  { messageIdentification :: Max35Text,
    creationDateTime :: ISODateTime,
    messageRecipient :: Maybe PartyIdentification32,
    messagePagination :: Maybe Pagination,
    additionalInformation :: Maybe Max500Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element GroupHeader42

data ImpliedCurrencyAmountRangeChoice
  = ImpliedCurrencyAmountRangeChoiceFrAmt {fromAmount :: AmountRangeBoundary1}
  | ImpliedCurrencyAmountRangeChoiceToAmt {toAmount :: AmountRangeBoundary1}
  | ImpliedCurrencyAmountRangeChoiceFrToAmt {fromToAmount :: FromToAmountRange}
  | ImpliedCurrencyAmountRangeChoiceEQAmt {equalAmount :: ImpliedCurrencyAndAmount}
  | ImpliedCurrencyAmountRangeChoiceNEQAmt {notEqualAmount :: ImpliedCurrencyAndAmount}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element ImpliedCurrencyAmountRangeChoice

data InterestType1Choice
  = InterestType1ChoiceCd {code :: InterestType1Code}
  | InterestType1ChoicePrtry {proprietary :: Max35Text}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element InterestType1Choice

data MessageIdentification2
  = MessageIdentification2
  { messageNameIdentification :: Maybe Max35Text,
    messageIdentification :: Maybe Max35Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element MessageIdentification2

data NameAndAddress10
  = NameAndAddress10 {name :: Max140Text, address :: PostalAddress6}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element NameAndAddress10

data NumberAndSumOfTransactions1
  = NumberAndSumOfTransactions1
  { numberOfEntries :: Maybe Max15NumericText,
    sum :: Maybe DecimalNumber
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element NumberAndSumOfTransactions1

data NumberAndSumOfTransactions2
  = NumberAndSumOfTransactions2
  { numberOfEntries :: Maybe Max15NumericText,
    sum :: Maybe DecimalNumber,
    totalNetEntryAmount :: Maybe DecimalNumber,
    creditDebitIndicator :: Maybe CreditDebitCode
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element NumberAndSumOfTransactions2

data OrganisationIdentification4
  = OrganisationIdentification4
  { elem_BICOrBEI :: Maybe AnyBICIdentifier,
    other :: [GenericOrganisationIdentification1]
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element OrganisationIdentification4

data OrganisationIdentificationSchemeName1Choice
  = OrganisationIdentificationSchemeName1ChoiceCd {code :: ExternalOrganisationIdentification1Code}
  | OrganisationIdentificationSchemeName1ChoicePrtry {proprietary :: Max35Text}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element OrganisationIdentificationSchemeName1Choice

data Pagination
  = Pagination
  { pageNumber :: Max5NumericText,
    lastPageIndicator :: YesNoIndicator
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element Pagination

data Party6Choice
  = Party6ChoiceOrgId {organisationIdentification :: OrganisationIdentification4}
  | Party6ChoicePrvtId {privateIdentification :: PersonIdentification5}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element Party6Choice

data PartyIdentification32
  = PartyIdentification32
  { name :: Maybe Max140Text,
    postalAddress :: Maybe PostalAddress6,
    identification :: Maybe Party6Choice,
    countryOfResidence :: Maybe CountryCode,
    contactDetails :: Maybe ContactDetails2
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element PartyIdentification32

data PersonIdentification5
  = PersonIdentification5
  { dateAndPlaceOfBirth :: Maybe DateAndPlaceOfBirth,
    other :: [GenericPersonIdentification1]
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element PersonIdentification5

data PersonIdentificationSchemeName1Choice
  = PersonIdentificationSchemeName1ChoiceCd {code :: ExternalPersonIdentification1Code}
  | PersonIdentificationSchemeName1ChoicePrtry {proprietary :: Max35Text}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element PersonIdentificationSchemeName1Choice

data PostalAddress6
  = PostalAddress6
  { addressType :: Maybe AddressType2Code,
    department :: Maybe Max70Text,
    subDepartment :: Maybe Max70Text,
    structureName :: Maybe Max70Text,
    buildingNumber :: Maybe Max16Text,
    postCode :: Maybe Max16Text,
    townName :: Maybe Max35Text,
    countrySubDivision :: Maybe Max35Text,
    country :: Maybe CountryCode,
    addressLine :: Restricted '[ 'MaxLength 7] [Max70Text]
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element PostalAddress6

data ProprietaryAgent2
  = ProprietaryAgent2
  { elem_type :: Max35Text,
    agent :: BranchAndFinancialInstitutionIdentification4
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element ProprietaryAgent2

data ProprietaryBankTransactionCodeStructure1
  = ProprietaryBankTransactionCodeStructure1
  { code :: Max35Text,
    issuer :: Maybe Max35Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element ProprietaryBankTransactionCodeStructure1

data ProprietaryDate2
  = ProprietaryDate2
  { elem_type :: Max35Text,
    date :: DateAndDateTimeChoice
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element ProprietaryDate2

data ProprietaryParty2
  = ProprietaryParty2
  { elem_type :: Max35Text,
    party :: PartyIdentification32
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element ProprietaryParty2

data ProprietaryPrice2
  = ProprietaryPrice2
  { elem_type :: Max35Text,
    price :: ActiveOrHistoricCurrencyAndAmount
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element ProprietaryPrice2

data ProprietaryQuantity1
  = ProprietaryQuantity1
  { elem_type :: Max35Text,
    quantity :: Max35Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element ProprietaryQuantity1

data ProprietaryReference1
  = ProprietaryReference1
  { elem_type :: Max35Text,
    reference :: Max35Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element ProprietaryReference1

data Purpose2Choice
  = Purpose2ChoiceCd {code :: ExternalPurpose1Code}
  | Purpose2ChoicePrtry {proprietary :: Max35Text}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element Purpose2Choice

data Rate3
  = Rate3
  { elem_type :: RateType4Choice,
    validityRange :: Maybe CurrencyAndAmountRange2
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element Rate3

data RateType4Choice
  = RateType4ChoicePctg {percentage :: PercentageRate}
  | RateType4ChoiceOthr {other :: Max35Text}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element RateType4Choice

data ReferredDocumentInformation3
  = ReferredDocumentInformation3
  { elem_type :: Maybe ReferredDocumentType2,
    number :: Maybe Max35Text,
    relatedDate :: Maybe ISODate
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element ReferredDocumentInformation3

data ReferredDocumentType1Choice
  = ReferredDocumentType1ChoiceCd {code :: DocumentType5Code}
  | ReferredDocumentType1ChoicePrtry {proprietary :: Max35Text}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element ReferredDocumentType1Choice

data ReferredDocumentType2
  = ReferredDocumentType2
  { codeOrProprietary :: ReferredDocumentType1Choice,
    issuer :: Maybe Max35Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element ReferredDocumentType2

data RemittanceAmount1
  = RemittanceAmount1
  { duePayableAmount :: Maybe ActiveOrHistoricCurrencyAndAmount,
    discountAppliedAmount :: Maybe ActiveOrHistoricCurrencyAndAmount,
    creditNoteAmount :: Maybe ActiveOrHistoricCurrencyAndAmount,
    taxAmount :: Maybe ActiveOrHistoricCurrencyAndAmount,
    adjustmentAmountAndReason :: [DocumentAdjustment1],
    remittedAmount :: Maybe ActiveOrHistoricCurrencyAndAmount
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element RemittanceAmount1

data RemittanceInformation5
  = RemittanceInformation5
  { unstructured :: [Max140Text],
    structured :: [StructuredRemittanceInformation7]
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element RemittanceInformation5

data RemittanceLocation2
  = RemittanceLocation2
  { remittanceIdentification :: Maybe Max35Text,
    remittanceLocationMethod :: Maybe RemittanceLocationMethod2Code,
    remittanceLocationElectronicAddress :: Maybe Max2048Text,
    remittanceLocationPostalAddress :: Maybe NameAndAddress10
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element RemittanceLocation2

data ReportEntry2
  = ReportEntry2
  { entryReference :: Maybe Max35Text,
    amount :: ActiveOrHistoricCurrencyAndAmount,
    creditDebitIndicator :: CreditDebitCode,
    reversalIndicator :: Maybe TrueFalseIndicator,
    status :: EntryStatus2Code,
    bookingDate :: Maybe DateAndDateTimeChoice,
    valueDate :: Maybe DateAndDateTimeChoice,
    accountServicerReference :: Maybe Max35Text,
    availability :: [CashBalanceAvailability2],
    bankTransactionCode :: BankTransactionCodeStructure4,
    commissionWaiverIndicator :: Maybe YesNoIndicator,
    additionalInformationIndicator :: Maybe MessageIdentification2,
    amountDetails :: Maybe AmountAndCurrencyExchange3,
    charges :: [ChargesInformation6],
    technicalInputChannel :: Maybe TechnicalInputChannel1Choice,
    interest :: [TransactionInterest2],
    entryDetails :: [EntryDetails1],
    additionalEntryInformation :: Maybe Max500Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element ReportEntry2

data ReportingSource1Choice
  = ReportingSource1ChoiceCd {code :: ExternalReportingSource1Code}
  | ReportingSource1ChoicePrtry {proprietary :: Max35Text}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element ReportingSource1Choice

data ReturnReason5Choice
  = ReturnReason5ChoiceCd {code :: ExternalReturnReason1Code}
  | ReturnReason5ChoicePrtry {proprietary :: Max35Text}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element ReturnReason5Choice

data ReturnReasonInformation10
  = ReturnReasonInformation10
  { originalBankTransactionCode :: Maybe BankTransactionCodeStructure4,
    originator :: Maybe PartyIdentification32,
    reason :: Maybe ReturnReason5Choice,
    additionalInformation :: [Max105Text]
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element ReturnReasonInformation10

data SecurityIdentification4Choice
  = SecurityIdentification4ChoiceISIN {elem_ISIN :: ISINIdentifier}
  | SecurityIdentification4ChoicePrtry {proprietary :: AlternateSecurityIdentification2}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element SecurityIdentification4Choice

data StructuredRemittanceInformation7
  = StructuredRemittanceInformation7
  { referredDocumentInformation :: [ReferredDocumentInformation3],
    referredDocumentAmount :: Maybe RemittanceAmount1,
    creditorReferenceInformation :: Maybe CreditorReferenceInformation2,
    invoicer :: Maybe PartyIdentification32,
    invoicee :: Maybe PartyIdentification32,
    additionalRemittanceInformation :: Restricted '[ 'MaxLength 3] [Max140Text]
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element StructuredRemittanceInformation7

data TaxAmount1
  = TaxAmount1
  { rate :: Maybe PercentageRate,
    taxableBaseAmount :: Maybe ActiveOrHistoricCurrencyAndAmount,
    totalAmount :: Maybe ActiveOrHistoricCurrencyAndAmount,
    details :: [TaxRecordDetails1]
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element TaxAmount1

data TaxAuthorisation1
  = TaxAuthorisation1
  { title :: Maybe Max35Text,
    name :: Maybe Max140Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element TaxAuthorisation1

data TaxCharges2
  = TaxCharges2
  { identification :: Maybe Max35Text,
    rate :: Maybe PercentageRate,
    amount :: Maybe ActiveOrHistoricCurrencyAndAmount
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element TaxCharges2

data TaxInformation3
  = TaxInformation3
  { creditor :: Maybe TaxParty1,
    debtor :: Maybe TaxParty2,
    administrationZone :: Maybe Max35Text,
    referenceNumber :: Maybe Max140Text,
    method :: Maybe Max35Text,
    totalTaxableBaseAmount :: Maybe ActiveOrHistoricCurrencyAndAmount,
    totalTaxAmount :: Maybe ActiveOrHistoricCurrencyAndAmount,
    date :: Maybe ISODate,
    sequenceNumber :: Maybe Number,
    record :: [TaxRecord1]
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element TaxInformation3

data TaxParty1
  = TaxParty1
  { taxIdentification :: Maybe Max35Text,
    registrationIdentification :: Maybe Max35Text,
    taxType :: Maybe Max35Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element TaxParty1

data TaxParty2
  = TaxParty2
  { taxIdentification :: Maybe Max35Text,
    registrationIdentification :: Maybe Max35Text,
    taxType :: Maybe Max35Text,
    authorisation :: Maybe TaxAuthorisation1
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element TaxParty2

data TaxPeriod1
  = TaxPeriod1
  { year :: Maybe ISODate,
    elem_type :: Maybe TaxRecordPeriod1Code,
    fromToDate :: Maybe DatePeriodDetails
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element TaxPeriod1

data TaxRecord1
  = TaxRecord1
  { elem_type :: Maybe Max35Text,
    category :: Maybe Max35Text,
    categoryDetails :: Maybe Max35Text,
    debtorStatus :: Maybe Max35Text,
    certificateIdentification :: Maybe Max35Text,
    formsCode :: Maybe Max35Text,
    period :: Maybe TaxPeriod1,
    taxAmount :: Maybe TaxAmount1,
    additionalInformation :: Maybe Max140Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element TaxRecord1

data TaxRecordDetails1
  = TaxRecordDetails1
  { period :: Maybe TaxPeriod1,
    amount :: ActiveOrHistoricCurrencyAndAmount
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element TaxRecordDetails1

data TechnicalInputChannel1Choice
  = TechnicalInputChannel1ChoiceCd {code :: ExternalTechnicalInputChannel1Code}
  | TechnicalInputChannel1ChoicePrtry {proprietary :: Max35Text}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element TechnicalInputChannel1Choice

data TotalTransactions2
  = TotalTransactions2
  { totalEntries :: Maybe NumberAndSumOfTransactions2,
    totalCreditEntries :: Maybe NumberAndSumOfTransactions1,
    totalDebitEntries :: Maybe NumberAndSumOfTransactions1,
    totalEntriesPerBankTransactionCode :: [TotalsPerBankTransactionCode2]
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element TotalTransactions2

data TotalsPerBankTransactionCode2
  = TotalsPerBankTransactionCode2
  { numberOfEntries :: Maybe Max15NumericText,
    sum :: Maybe DecimalNumber,
    totalNetEntryAmount :: Maybe DecimalNumber,
    creditDebitIndicator :: Maybe CreditDebitCode,
    forecastIndicator :: Maybe TrueFalseIndicator,
    bankTransactionCode :: BankTransactionCodeStructure4,
    availability :: [CashBalanceAvailability2]
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element TotalsPerBankTransactionCode2

data TransactionAgents2
  = TransactionAgents2
  { debtorAgent :: Maybe BranchAndFinancialInstitutionIdentification4,
    creditorAgent :: Maybe BranchAndFinancialInstitutionIdentification4,
    intermediaryAgent1 :: Maybe BranchAndFinancialInstitutionIdentification4,
    intermediaryAgent2 :: Maybe BranchAndFinancialInstitutionIdentification4,
    intermediaryAgent3 :: Maybe BranchAndFinancialInstitutionIdentification4,
    receivingAgent :: Maybe BranchAndFinancialInstitutionIdentification4,
    deliveringAgent :: Maybe BranchAndFinancialInstitutionIdentification4,
    issuingAgent :: Maybe BranchAndFinancialInstitutionIdentification4,
    settlementPlace :: Maybe BranchAndFinancialInstitutionIdentification4,
    proprietary :: [ProprietaryAgent2]
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element TransactionAgents2

data TransactionDates2
  = TransactionDates2
  { acceptanceDateTime :: Maybe ISODateTime,
    tradeActivityContractualSettlementDate :: Maybe ISODate,
    tradeDate :: Maybe ISODate,
    interBankSettlementDate :: Maybe ISODate,
    startDate :: Maybe ISODate,
    endDate :: Maybe ISODate,
    transactionDateTime :: Maybe ISODateTime,
    proprietary :: [ProprietaryDate2]
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element TransactionDates2

data TransactionInterest2
  = TransactionInterest2
  { amount :: ActiveOrHistoricCurrencyAndAmount,
    creditDebitIndicator :: CreditDebitCode,
    elem_type :: Maybe InterestType1Choice,
    rate :: [Rate3],
    fromToDate :: Maybe DateTimePeriodDetails,
    reason :: Maybe Max35Text
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element TransactionInterest2

data TransactionParty2
  = TransactionParty2
  { initiatingParty :: Maybe PartyIdentification32,
    debtor :: Maybe PartyIdentification32,
    debtorAccount :: Maybe CashAccount16,
    ultimateDebtor :: Maybe PartyIdentification32,
    creditor :: Maybe PartyIdentification32,
    creditorAccount :: Maybe CashAccount16,
    ultimateCreditor :: Maybe PartyIdentification32,
    tradingParty :: Maybe PartyIdentification32,
    proprietary :: [ProprietaryParty2]
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element TransactionParty2

data TransactionPrice2Choice
  = TransactionPrice2ChoiceDealPric {dealPrice :: ActiveOrHistoricCurrencyAndAmount}
  | TransactionPrice2ChoicePrtry {proprietary :: NonEmpty ProprietaryPrice2}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element TransactionPrice2Choice

data TransactionQuantities1Choice
  = TransactionQuantities1ChoiceQty {quantity :: FinancialInstrumentQuantityChoice}
  | TransactionQuantities1ChoicePrtry {proprietary :: ProprietaryQuantity1}
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element TransactionQuantities1Choice

data TransactionReferences2
  = TransactionReferences2
  { messageIdentification :: Maybe Max35Text,
    accountServicerReference :: Maybe Max35Text,
    paymentInformationIdentification :: Maybe Max35Text,
    instructionIdentification :: Maybe Max35Text,
    endToEndIdentification :: Maybe Max35Text,
    transactionIdentification :: Maybe Max35Text,
    mandateIdentification :: Maybe Max35Text,
    chequeNumber :: Maybe Max35Text,
    clearingSystemReference :: Maybe Max35Text,
    proprietary :: Maybe ProprietaryReference1
  }
  deriving stock (Generic)
  deriving (FromElement, ToElement) via Camt53Element TransactionReferences2
-}
