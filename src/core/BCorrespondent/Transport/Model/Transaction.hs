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
       ( OkTransaction (..),
         TransactionId (..),
         AbortedTransactionRequest (..),
         FailedTransaction (..),
         encodeOkTransaction
       ) where

import BCorrespondent.Transport.Model.Invoice (Currency, Fee)
import Data.UUID (UUID)
import Data.Aeson (ToJSON, FromJSON, encode)
import Data.Aeson.Generic.DerivingVia
import Data.Tuple.Extended (app18, app17, del2)
import GHC.Generics
import Data.Swagger (ToSchema)
import Test.QuickCheck.Extended (Arbitrary)
import Database.Transaction (ParamsShow)
import qualified Data.Text as T
import TH.Mk (mkEncoder, mkArbitrary)
import Data.Maybe (fromMaybe)
import Database.Transaction (ParamsShow (..))
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeOfDay)
import Data.Time.Calendar.OrdinalDate (Day)
import Data.String.Conv (toS)


{-
  "ident": "579b254b-dd5d-40a6-9377-beb6d3af98a3",
  "transactionId": "TCKAFUSD000000006",
   "sender": "...",
   "city”: "...",
   "country”: "...",
   "senderBankName”: "...",
   "senderSwiftSepaCode": "...",
   "transactionOperationCode": "...",
   "amount": 12.34,
   "currency": "usd",
   "charges": "our",
   "receiverBankName”: "...",
   "receiverSwiftSepaCode": "...",
   "correspondentBank": "...",
   "correspondentBankSwiftSepaCode": "...",
   "transactionTime": "00:00:00",
   "transactionDate": "1970-01-01",
   "transactionDescription": "..."
-}
data OkTransaction =
     OkTransaction 
     { -- | there is an external ident that is sent previously in the invoice request. 
        -- you simply take it from the invoice request and forward it in the webhook
       okIdent :: !UUID,
       okTransactionId :: !T.Text,
       sender :: !T.Text,
       city :: !T.Text,
       country :: !T.Text,
       senderBankName :: !T.Text,
      -- either it's obtained from 
      -- swift (https://www.swift.com) or 
      -- sepa (https://finance.ec.europa.eu/consumer-finance-and-payments/payment-services/single-euro-payments-area-sepa_en) 
      -- agent
       senderSwiftOrSepaCode :: !T.Text,
       transactionOperationCode :: !T.Text, 
       receiverBankName :: !T.Text,
       receiverSwiftOrSepaCode :: !T.Text,
       correspondentBank :: !T.Text,
       correspondentBankSwiftOrSepaCode :: !T.Text,
       transactionDate :: !Day,
       transactionTime :: !TimeOfDay,
       transactionDescription :: !T.Text,
       amount :: !Double,
       currency :: !Currency, 
       fee :: !Fee
     }
     deriving stock (Generic, Show)
     deriving (ToJSON, FromJSON)
       via WithOptions 
        '[ FieldLabelModifier 
          '[UserDefined FirstLetterToLower,
            UserDefined (StripPrefix "ok")]]
       OkTransaction

mkEncoder ''OkTransaction
mkArbitrary ''OkTransaction

encodeOkTransaction 
  :: OkTransaction
  -> (UUID, T.Text, T.Text, T.Text, 
      T.Text, T.Text, T.Text, T.Text, T.Text, 
      T.Text, T.Text, Day, TimeOfDay, T.Text,
      Double, T.Text, T.Text)
encodeOkTransaction = 
    fromMaybe undefined 
  . fmap (del2 . app17 (toS. encode) . app18 (toS. encode))
  . mkEncoderOkTransaction
{-# INLINE encodeOkTransaction #-}

instance ParamsShow OkTransaction where
  render = render . encodeOkTransaction

newtype TransactionId = TransactionId UUID
  deriving stock (Generic, Show)
  deriving newtype (ToJSON, FromJSON, Arbitrary, ParamsShow)
 
instance ToSchema TransactionId

data AbortedTransactionRequest = AbortedTransactionRequest T.Text
    deriving stock (Generic, Show)
    deriving (ToJSON) via WithOptions DefaultOptions AbortedTransactionRequest


data FailedTransaction =
     FailedTransaction 
     { failedIdent :: !UUID,
       failedTransactionId :: !T.Text,
       reason :: !T.Text,
       timestamp :: !UTCTime
     }
     deriving stock (Generic, Show)
     deriving (ToJSON, FromJSON)
       via WithOptions 
        '[ FieldLabelModifier 
          '[UserDefined FirstLetterToLower,
            UserDefined (StripPrefix "failed")]]
       FailedTransaction