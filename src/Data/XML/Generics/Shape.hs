{-# LANGUAGE UndecidableInstances #-}

module Data.XML.Generics.Shape where

import Data.Kind
import GHC.TypeLits
import Generics.SOP.Type.Metadata

data Shape = RecordShape | SumShape

type family ComputeShape (info :: DatatypeInfo) (code :: [[Type]]) :: Shape where
  ComputeShape info code = ComputeShape' (IsRecord info) (IsSum info code)

type family ComputeShape' (isRecord :: Bool) (isSum :: Bool) :: Shape where
  ComputeShape' 'False 'False = TypeError ('Text "Not a simple shape (record/sum)")
  ComputeShape' 'True 'False = 'RecordShape
  ComputeShape' 'False 'True = 'SumShape
  ComputeShape' 'True 'True = TypeError ('Text "Both record and sum?")

type family IsRecord (info :: DatatypeInfo) :: Bool where
  IsRecord ('Newtype _ _ info) = IsRecordConstructor info
  IsRecord ('ADT _ _ info _) = AllRecordConstructors info

type family IsRecordConstructor (info :: ConstructorInfo) :: Bool where
  IsRecordConstructor ('Record _ _) = 'True
  IsRecordConstructor _ = 'False

type family AllRecordConstructors (infos :: [ConstructorInfo]) :: Bool where
  AllRecordConstructors '[] = 'True
  AllRecordConstructors (info ': infos) = And (IsRecordConstructor info) (AllRecordConstructors infos)

type family And (a :: Bool) (b :: Bool) :: Bool where
  And 'True 'True = 'True
  And _ _ = 'False

type family IsSum (info :: DatatypeInfo) (code :: [[Type]]) :: Bool where
  IsSum ('Newtype _ _ info) code = And (AllSingletons code) (IsSumConstructor info)
  IsSum ('ADT _ _ info _) code = And (AllSingletons code) (AllSumConstructors info)

type family AllSingletons (code :: [[Type]]) :: Bool where
  AllSingletons '[] = 'True
  AllSingletons ('[_] ': rest) = AllSingletons rest
  AllSingletons _ = 'False

type family IsSumConstructor (info :: ConstructorInfo) :: Bool where
  IsSumConstructor ('Constructor _) = 'True
  IsSumConstructor _ = 'False

type family AllSumConstructors (infos :: [ConstructorInfo]) :: Bool where
  AllSumConstructors '[] = 'True
  AllSumConstructors (info ': infos) = And (IsSumConstructor info) (AllSumConstructors infos)
