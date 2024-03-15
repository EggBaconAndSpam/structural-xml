module Data.XML.ISO20022.Types where

-- ISO8601 timezoned (xs:dateTime technically allows non-timezoned but that's ridiculous)
newtype ISODateTime = ISODateTime {unISODateTime :: UTCTime}
  deriving (FromElement, ToElement) via ContentElement ISODateTime

instance FromContent ISODateTime where
  fromContent t i = case iso8601ParseM $ Text.unpack t of
    Nothing -> Left . parserError i $ "Not a valid timestamp in ISO8601 format: " <> Text.unpack t
    Just u -> pure . ISODateTime $ zonedTimeToUTC u

instance ToContent UTCTime where
  toContent = Text.pack . iso8601Show

deriving via ContentElement ISODateTime instance FromElement ISODateTime

-- ISO8601 Day (xs:date also allows timezones)
newtype ISODate = ISODate {unISODate :: Day}
  deriving (FromElement, ToElement) via ContentElement ISODate

instance FromContent ISODate where
  fromContent t i = case iso8601ParseM $ Text.unpack t of
    Nothing -> Left . parserError i $ "Not a valid date in ISO8601 format: " <> Text.unpack t
    Just d -> pure $ ISODate d

instance ToContent ISODate where
  toContent = Text.pack . iso8601Show

-- xs:boolean also allows 0/1
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

type YesNoIndiciator = ISOBool
