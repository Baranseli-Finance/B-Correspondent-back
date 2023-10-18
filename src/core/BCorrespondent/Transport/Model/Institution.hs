{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module BCorrespondent.Transport.Model.Institution 
       ( Withdraw, 
         Balance (..),
         WithdrawalHistoryItem,
         WithdrawalStatus (..),
         InitWithdrawal (..)
       ) where

import BCorrespondent.Transport.Model.Invoice (Currency)
import Data.Aeson.Generic.DerivingVia
import Data.Proxy (Proxy (..))
import Data.Swagger.Schema.Extended 
      ( deriveToSchemaFieldLabelModifier, 
        firstLetterModify, 
        deriveToSchemaConstructorTag
      )
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Data.Int (Int64)
import Data.Char (toLower)
import Data.Time.Clock (UTCTime)

data Withdraw = 
     Withdraw 
     { withdrawWalletIdent :: !Int64,
       withdrawAmount :: !Double
     }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined
              (StripConstructor 
               Withdraw)]]
      Withdraw

deriveToSchemaFieldLabelModifier ''Withdraw [|firstLetterModify (Proxy @Withdraw)|]

data Balance = 
     Balance 
     { balanceWalletIdent :: !Int64,
       balanceCurrency :: !Currency,
       balanceAmount :: !Double 
     }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined
              (StripConstructor 
               Balance)]]
      Balance

deriveToSchemaFieldLabelModifier ''Balance [|firstLetterModify (Proxy @Balance)|]

data WithdrawalStatus = Processing | Ok | Declined
     deriving stock (Generic, Show)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[ConstructorTagModifier 
            '[UserDefined ToLower]]
         WithdrawalStatus

deriveToSchemaConstructorTag ''WithdrawalStatus [| map toLower |]

data WithdrawalHistoryItem = 
     WithdrawalHistoryItem 
     { withdrawalHistoryItemIdent :: !Int64,
       withdrawalHistoryItemCurrency :: Currency,
       withdrawalHistoryItemAmount :: !Double,
       withdrawalHistoryItemWithdrawalStatus :: !WithdrawalStatus,
       withdrawalHistoryItemCreated :: !UTCTime
     }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined
              (StripConstructor 
               WithdrawalHistoryItem)]]
      WithdrawalHistoryItem

deriveToSchemaFieldLabelModifier ''WithdrawalHistoryItem [|firstLetterModify (Proxy @WithdrawalHistoryItem)|]

data InitWithdrawal = 
     InitWithdrawal 
     {
      initWithdrawalWalletBalances :: ![Balance],
      initWithdrawalHistory :: ![WithdrawalHistoryItem]
     }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined
              (StripConstructor 
               InitWithdrawal)]]
      InitWithdrawal

deriveToSchemaFieldLabelModifier ''InitWithdrawal [|firstLetterModify (Proxy @InitWithdrawal)|]