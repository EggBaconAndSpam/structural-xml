module Data.XML.Helpers.ContentElement (ContentElement (..)) where

import GHC.Generics

-- | This newtype wrapper can be use to turn 'simple types' into 'complex types':
--
-- newtype AllCapsText = AllCapsText Text
--
-- instance FromContent AllCapsText where
--  fromContent = undefined  -- bespoke
--
-- instance ToContent AllCapsText where
--  toContent (AllCapsText t) = t
--
-- deriving via ContentElement AllCapsText instance FromElement AllCapsText
-- deriving via ContentElement AllCapsText instance ToElement AllCapsText
newtype ContentElement a = ContentElement {content :: a}
  deriving stock (Show, Generic)
