{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS -ddump-splices -ddump-to-file #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test where

import Data.Time
import Data.XML.BoundedList
import Data.XML.Camt53

$(qCamt53Schema)
