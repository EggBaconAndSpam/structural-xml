module Data.XML.ISO20022.Types where

import Control.Applicative
import qualified Data.Text as Text
import Data.Time
import Data.Time.Format.ISO8601
import Data.XML

-- xs:dateTime allows both timezoned and non-timezoned...
data ISODateTime = ISODateTime UTCTime | ISODateTimeLocal LocalTime
  deriving (FromElement, ToElement) via ContentElement ISODateTime

instance FromContent ISODateTime where
  fromContent t i = do
    let t' = Text.unpack t
    maybe
      (Left . parserError i $ "Not a valid timestamp in ISO8601 format: " <> Text.unpack t)
      pure
      ( ISODateTime . zonedTimeToUTC <$> iso8601ParseM t'
          <|> ISODateTime <$> iso8601ParseM t'
          <|> ISODateTimeLocal <$> iso8601ParseM t'
      )

instance ToContent ISODateTime where
  toContent (ISODateTime t) = Text.pack $ iso8601Show t
  toContent (ISODateTimeLocal t) = Text.pack $ iso8601Show t

-- ISO8601 Day (xs:date also allows timezones but ISO20022 doesn't)
newtype ISODate = ISODate {unISODate :: Day}
  deriving (FromElement, ToElement) via ContentElement ISODate

instance FromContent ISODate where
  fromContent t i = case iso8601ParseM $ Text.unpack t of
    Nothing -> Left . parserError i $ "Not a valid date in ISO8601 format: " <> Text.unpack t
    Just d -> pure $ ISODate d

instance ToContent ISODate where
  toContent (ISODate d) = Text.pack $ iso8601Show d

-- xs:boolean also allows 0/1?
newtype ISOBool = ISOBool {unISOBool :: Bool}
  deriving (FromElement, ToElement) via ContentElement ISOBool

instance FromContent ISOBool where
  fromContent "true" _ = pure $ ISOBool True
  fromContent "false" _ = pure $ ISOBool False
  fromContent t i =
    Left . parserError i $
      "Failed to read \""
        <> Text.unpack t
        <> "\" as an ISO20022 boolean"

instance ToContent ISOBool where
  toContent (ISOBool True) = "true"
  toContent (ISOBool False) = "false"

type TrueFalseIndicator = ISOBool

type YesNoIndicator = ISOBool

type ChargeIncludedIndicator = ISOBool
