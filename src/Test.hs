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
import Data.XML.BoundedList
import Data.XML.Camt53
import Data.XML.Parse.Generically.Labelled
import GHC.Generics

$(qCamt53Schema)
