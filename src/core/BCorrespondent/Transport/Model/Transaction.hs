{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Transport.Model.Transaction 
       ( TransactionFromPaymentProvider (..),
         TransactionId (..),
         WireTransferAgent (..),
         BankOperationCode (..),
         AbortedTransactionRequest (..),
         encodeTransactionFromPaymentProvider
       ) where

import BCorrespondent.Transport.Model.Invoice (Currency, Fee)
import Data.UUID (UUID)
import Data.Aeson (ToJSON, FromJSON, encode)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics
import Data.Swagger (ToSchema)
import Test.QuickCheck.Extended (Arbitrary)
import Database.Transaction (ParamsShow)
import qualified Data.Text as T
import TH.Mk (mkEncoder, mkArbitrary)
import Data.Tuple.Extended (app9, app7, app13, app16, del2)
import Data.Maybe (fromMaybe)
import Database.Transaction (ParamsShow (..))
import Data.String.Conv (toS)
import Data.Time.Clock (UTCTime)


data WireTransferAgent =
        -- https://www.swift.com/
        Swift
        -- https://finance.ec.europa.eu/consumer-finance-and-payments/payment-services/single-euro-payments-area-sepa_en
      | Sepa
     deriving stock (Generic, Show)
     deriving
     (FromJSON, ToJSON)
     via WithOptions
          '[SumEnc UntaggedVal,
            ConstructorTagModifier
            '[UserDefined ToLower]]
         WireTransferAgent

mkArbitrary ''WireTransferAgent

-- :23B::Bank operation code

-- Field description
-- Use this four-character field to specify the type of operation to which your instruction relates.

-- The possible values are:
--   CRED for a credit transfer that involves no SWIFT Service Level;
--   SPAY for a credit transfer to be processed according to the SWIFT Pay Service Level;
--   SSTD for a credit transfer to be processed according to the SWIFT Standard Service Level; and
--   SPRI  for a credit transfer to be processed according to the SWIFT Priority Service Level.

-- Validation rule
-- This field is mandatory.

-- Format
-- 4!c

-- MT

-- MT 103 Single customer credit transfer (SWIFT) (Wire transfer instruction) 
-- https://my.euroclear.com/drm/DRM_HTML_Mega/SWIFT/Money_Transfer/PAGES/3671391350476F6F.HTM
-- MT 103 Book transfer
-- https://my.euroclear.com/drm/DRM_HTML_Mega/SWIFT/Money_Transfer/PAGES/367160125047550B.HTM
data BankOperationCode = Cred | Unknown
     deriving stock (Generic, Show)
     deriving
     (FromJSON, ToJSON)
     via WithOptions
          '[SumEnc UntaggedVal,
            ConstructorTagModifier 
            '[UserDefined ToUpper]]
         BankOperationCode

mkArbitrary ''BankOperationCode

{-
-- description msg pattern; <b><c>{trid}</c><m>{msg}</m><b>

       "ident": "36fab9bc-40d4-4975-887b-c729edf6cd18",
       "transactionId": "TCKAFUSD000000006",
       "sender": "...",
       "city": "NY",
       "country": "USA"
       "senderBank": "Bank of NewYork",
       "senderWireTransferAgent": "swift",
       "senderTransferAgentCode": "IRVTUS3N",
       "bankOperationCode": "CRED", 
       "receiverBank": "Vakifbank",
       "receiverWireTransferAgentCode": "TVBATR2AFEX",
       "amount": 234.89,
       "currency": "usd",
       "correspondentBank": "BankOne",
       "correspondentBankWireTransferAgentCode": "BKONMUMU",
       "charges": "our",
       "timestamp": "2016-07-22T00:00:00Z",
       "description": "Payment for invoice 123445 for software development services"
-}
data TransactionFromPaymentProvider =
     TransactionFromPaymentProvider 
     { -- | there is an external ident that is sent previously in the invoice request. 
        -- you simply take it from the invoice request and forward it in the webhook
       transactionFromPaymentProviderIdent :: !UUID,
       transactionFromPaymentProviderTransactionId :: !T.Text,
       transactionFromPaymentProviderSender :: !T.Text,
       transactionFromPaymentProviderCity :: !T.Text,
       transactionFromPaymentProviderCountry :: !T.Text,
       transactionFromPaymentProviderSenderBank :: !T.Text,
       transactionFromPaymentProviderSenderWireTransferAgent :: !WireTransferAgent,
       transactionFromPaymentProviderSenderTransferAgentCode :: !T.Text,
       transactionFromPaymentProviderBankOperationCode :: !BankOperationCode, 
       transactionFromPaymentProviderReceiverBank :: !T.Text,
       transactionFromPaymentProviderReceiverWireTransferAgentCode :: !T.Text,
       transactionFromPaymentProviderAmount :: !Double,
       transactionFromPaymentProviderCurrency :: !Currency,
       transactionFromPaymentProviderCorrespondentBank :: !T.Text,
       transactionFromPaymentProviderCorrespondentBankWireTransferAgentCode :: !T.Text,
       transactionFromPaymentProviderCharges :: !Fee,
       transactionFromPaymentProviderTimestamp :: !UTCTime,
       transactionFromPaymentProviderDescription :: !T.Text
     }
     deriving stock (Generic, Show)
     deriving (ToJSON, FromJSON)
       via WithOptions
          '[OmitNothingFields 'True, 
            FieldLabelModifier 
            '[UserDefined FirstLetterToLower, 
              UserDefined (StripConstructor TransactionFromPaymentProvider)]]
          TransactionFromPaymentProvider

mkEncoder ''TransactionFromPaymentProvider
mkArbitrary ''TransactionFromPaymentProvider

encodeTransactionFromPaymentProvider 
  :: TransactionFromPaymentProvider
  -> (UUID, T.Text, T.Text,
      T.Text, T.Text, T.Text, T.Text, T.Text, T.Text, T.Text, 
      Double, T.Text, T.Text, T.Text, T.Text, UTCTime, T.Text)
encodeTransactionFromPaymentProvider =
    fromMaybe undefined
  . fmap (
        del2
      . app7 (toS . show) 
      . app9 (toS . show)
      . app13 (toS . encode)
      . app16 (toS . encode))
  . mkEncoderTransactionFromPaymentProvider

instance ParamsShow TransactionFromPaymentProvider where
  render = render . encodeTransactionFromPaymentProvider

newtype TransactionId = TransactionId UUID
  deriving stock (Generic, Show)
  deriving newtype (ToJSON, FromJSON, Arbitrary, ParamsShow)
 
instance ToSchema TransactionId

data AbortedTransactionRequest = AbortedTransactionRequest T.Text
    deriving stock (Generic, Show)
    deriving (ToJSON) via WithOptions DefaultOptions AbortedTransactionRequest