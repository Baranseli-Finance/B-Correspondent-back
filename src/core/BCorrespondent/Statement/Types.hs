{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BCorrespondent.Statement.Types where

import Data.Word (Word8, Word32)
import Test.QuickCheck.Extended (Arbitrary (..), chooseInt)
import Database.Transaction (ParamsShow (..))
import Data.Aeson (FromJSON, ToJSON)

newtype Hour = Hour Word8
  deriving newtype Num
  deriving Show

instance ParamsShow Hour where
  render (Hour x) = show x

instance Arbitrary Hour where
  arbitrary = fmap fromIntegral $ chooseInt (0, 23)

newtype Min = Min Word8
  deriving newtype Num
  deriving Show

instance ParamsShow Min where
  render (Min x) = show x

instance Arbitrary Min where
  arbitrary = fmap fromIntegral $ chooseInt (0, 59)

newtype Sec = Sec Word8
  deriving newtype Num
  deriving Show

instance ParamsShow Sec where
  render (Sec x) = show x

instance Arbitrary Sec where
  arbitrary = fmap fromIntegral $ chooseInt (0, 59)

newtype Year = Year Word32
  deriving newtype (Num, ToJSON, FromJSON)
  deriving (Show, Ord, Eq)

instance ParamsShow Year where
  render (Year x) = show x

instance Arbitrary Year where
  arbitrary = fmap fromIntegral $ chooseInt (0, 2000)

newtype Month = Month Word8
  deriving newtype (Num, ToJSON, FromJSON)
  deriving (Show, Ord, Eq)

instance ParamsShow Month where
  render (Month x) = show x

instance Arbitrary Month where
  arbitrary = fmap fromIntegral $ chooseInt (1, 11)

newtype Day = Day Word8
  deriving newtype (Num, ToJSON, FromJSON)
  deriving (Show, Ord, Eq)

instance ParamsShow Day where
  render (Day x) = show x

instance Arbitrary Day where
  arbitrary = fmap fromIntegral $ chooseInt (1, 31)

newtype DoY = DoY Word32
  deriving newtype (Num, ToJSON, FromJSON)
  deriving (Show, Ord, Eq)

instance ParamsShow DoY where
  render (DoY x) = show x

instance Arbitrary DoY where
  arbitrary = fmap fromIntegral $ chooseInt (1, 365)
