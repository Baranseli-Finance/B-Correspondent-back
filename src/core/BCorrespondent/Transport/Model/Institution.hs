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
       ( Withdraw (..), 
         Balance (..),
         WithdrawalHistoryItem,
         WithdrawalStatus (..),
         InitWithdrawal (..),
         WithdrawResult (..),
         WithdrawResultStatus (..),
         WithdrawalHistory (..),
         WithdrawalPaymentProviderRequest 
           (WithdrawalPaymentProviderRequest),
         WithdrawalPaymentProviderResponse (..),
         WithdrawalPaymentProviderResponseStatus (..),
         WithdrawalCode (..)
       ) where

import BCorrespondent.Transport.Model.Invoice (Currency)
import Data.Aeson.Generic.DerivingVia
import Data.Proxy (Proxy (..))
import Data.Swagger.Schema.Extended 
      ( deriveToSchemaFieldLabelModifier, 
        firstLetterModify, 
        deriveToSchemaConstructorTag,
        deriveToSchema
      )
import Data.Aeson (ToJSON, FromJSON, toJSON)
import GHC.Generics (Generic)
import Data.Int (Int64, Int32)
import Data.Char (toLower)
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import Database.Transaction (ParamsShow (..))
import Data.UUID (UUID)
import TH.Mk (mkArbitrary)


data WithdrawalCode = WithdrawalCode { code :: Int32 }
    deriving stock (Generic, Show)
    deriving 
      (FromJSON, ToJSON)
      via WithOptions DefaultOptions WithdrawalCode

deriveToSchema ''WithdrawalCode

mkArbitrary ''WithdrawalCode

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

data WithdrawResultStatus = 
       NotEnoughFunds 
     | WithdrawalRegistered
     | FrozenFunds
     deriving stock (Generic, Show)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[ConstructorTagModifier 
            '[UserDefined ToLower]]
         WithdrawResultStatus

deriveToSchemaConstructorTag ''WithdrawResultStatus [| map toLower |]

data WithdrawResult =
     WithdrawResult
     { withdrawResultStatus :: !WithdrawResultStatus,
       withdrawResultFrozenFunds :: !(Maybe Double) 
     } 
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[OmitNothingFields 'True,
            FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined
              (StripConstructor 
               WithdrawResult)]]
      WithdrawResult

deriveToSchemaFieldLabelModifier ''WithdrawResult [|firstLetterModify (Proxy @WithdrawResult)|]

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

data WithdrawalStatus =
       -- the withdrawal is registered in the server
       Registered
      -- handed over to payment provider for processing
     | Processing 
     | Confirmed
     | Declined
     deriving stock (Generic, Show)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[ConstructorTagModifier 
            '[UserDefined ToLower]]
         WithdrawalStatus

deriveToSchemaConstructorTag ''WithdrawalStatus [| map toLower |]

instance ParamsShow WithdrawalStatus where
  render = show . toJSON

mkArbitrary ''WithdrawalStatus

data WithdrawalHistoryItem = 
     WithdrawalHistoryItem 
     { withdrawalHistoryItemInitiator :: Text,
       withdrawalHistoryItemIdent :: !Int64,
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

data WithdrawalHistory = 
     WithdrawalHistory
     { withdrawalHistoryTotal :: !Int64,
       withdrawalHistoryItems :: ![WithdrawalHistoryItem] 
     }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined
              (StripConstructor 
               WithdrawalHistory)]]
      WithdrawalHistory

deriveToSchemaFieldLabelModifier ''WithdrawalHistory [|firstLetterModify (Proxy @WithdrawalHistory)|]

data InitWithdrawal = 
     InitWithdrawal 
     {
      initWithdrawalWalletBalances :: ![Balance],
      initWithdrawalHistory :: !WithdrawalHistory
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


data WithdrawalPaymentProviderRequest =
     WithdrawalPaymentProviderRequest
     { withdrawalPaymentProviderRequestIdentificator :: !Text,
       withdrawalPaymentProviderRequestAmount :: !Double,
       withdrawalPaymentProviderRequestExternalId :: !UUID
     }
    deriving stock (Generic, Show)
    deriving
      (ToJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined
              (StripConstructor 
               WithdrawalPaymentProviderRequest)]]
      WithdrawalPaymentProviderRequest
    
data WithdrawalPaymentProviderResponseStatus = 
         WithdrawalPaymentProviderResponseStatusOk
       | WithdrawalPaymentProviderResponseStatusDeclined
     deriving stock (Generic, Show, Eq)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[SumEnc UntaggedVal,
            ConstructorTagModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructorNullary 
                WithdrawalPaymentProviderResponseStatus)]]
         WithdrawalPaymentProviderResponseStatus

data WithdrawalPaymentProviderResponse = 
        WithdrawalPaymentProviderResponse 
        { withdrawalPaymentProviderResponseStatus 
            :: !WithdrawalPaymentProviderResponseStatus,
          withdrawalPaymentProviderResponseExternalId :: !UUID
        }
    deriving stock (Generic, Show)
    deriving
      (FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined
              (StripConstructor 
               WithdrawalPaymentProviderResponse)]]
      WithdrawalPaymentProviderResponse