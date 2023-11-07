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
         TransactionToInitiator,
         encodeTransactionFromPaymentProvider
       ) where

import BCorrespondent.Transport.Model.Invoice (Currency)
import Data.UUID (UUID)
import Data.Aeson (ToJSON, FromJSON, encode)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier, modify)
import Data.Proxy (Proxy (..))
import Data.Swagger (ToSchema)
import Test.QuickCheck.Extended (Arbitrary)
import Database.Transaction (ParamsShow)
import qualified Data.Text as T
import TH.Mk (mkEncoder, mkArbitrary)
import Data.Tuple.Extended (del12, app9)
import Data.Maybe (fromMaybe)
import Database.Transaction (ParamsShow (..))
import Data.String.Conv (toS)


-- {
--     "ident": "579b254b-dd5d-40a6-9377-beb6d3af98a3",
--     "sender": {
--         "name": "",
--         "country": "",
--         "city": "",
--         "address": "",
--         "phone": "",
--         "bank": {
--             "name": "",
--             "country": "",
--             "city": ""
--         }
--     },
--     "recipient": {
--         "name": "",
--         "country": "",
--         "city": "",
--         "address": "",
--         "phone": "..."
--     },
--     "bankAccount": "",
--     "amount": "",
--     "currency": "",
--     // the purpose of a payment
--     "description": "",
--     // who's responsibility for transaction expenses, 
--     // enum: our | sha
--     "expenses": "",
--     "swiftSepaCode": "",
--     // full message issued by swift
--     "swiftMessage": "",
--     "correspondentBank": {
--         "name": "",
--         "country": "",
--         "city": ""
--     }
-- }
data TransactionFromPaymentProvider =
     TransactionFromPaymentProvider 
     { -- | there is an external ident that is sent previously in the invoice request. 
        -- you simply take it from the invoice request and forward it in the webhook
       transactionFromPaymentProviderIdent :: !UUID,
       transactionFromPaymentProviderSender :: !T.Text,
       transactionFromPaymentProviderAddress :: !T.Text,
       transactionFromPaymentProviderPhoneNumber :: !T.Text,
       transactionFromPaymentProviderBank :: !T.Text,
       transactionFromPaymentProviderSwfitSepaCode :: !T.Text,
       transactionFromPaymentProviderBankAccount :: !T.Text,
       transactionFromPaymentProviderAmount :: !Double,
       transactionFromPaymentProviderCurrency :: !Currency,
       transactionFromPaymentProviderCorrespondentBank :: !T.Text,
       transactionFromPaymentProviderSwfitSepaCodeCorrespondentBank :: !T.Text,
       -- base64 encoded message issued by swift
       transactionFromPaymentProviderSwiftMessage :: !T.Text
     }
     deriving stock (Generic, Show)
     deriving (ToJSON, FromJSON)
       via WithOptions
          '[OmitNothingFields 'True, 
            FieldLabelModifier 
            '[UserDefined FirstLetterToLower, 
              UserDefined (StripConstructor TransactionFromPaymentProvider)]]
          TransactionFromPaymentProvider

deriveToSchemaFieldLabelModifier ''TransactionFromPaymentProvider [|modify (Proxy @TransactionFromPaymentProvider)|]

mkEncoder ''TransactionFromPaymentProvider
mkArbitrary ''TransactionFromPaymentProvider

encodeTransactionFromPaymentProvider 
  :: TransactionFromPaymentProvider
  -> (UUID, T.Text, T.Text, T.Text, 
      T.Text, T.Text, T.Text, 
      Double, T.Text, T.Text, T.Text)
encodeTransactionFromPaymentProvider = 
    fromMaybe undefined 
  . fmap (app9 (toS @_ @T.Text . encode) . del12)
  . mkEncoderTransactionFromPaymentProvider

instance ParamsShow TransactionFromPaymentProvider where
  render = render . encodeTransactionFromPaymentProvider

newtype TransactionId = TransactionId UUID
  deriving stock (Generic, Show)
  deriving newtype (ToJSON, FromJSON, Arbitrary, ParamsShow)
 
instance ToSchema TransactionId

data TransactionToInitiator =
     TransactionToInitiator 
     { transactionToInitiatorSenderName :: T.Text }
     deriving stock (Generic, Show)
     deriving (ToJSON, FromJSON)
       via WithOptions
          '[OmitNothingFields 'True, 
            FieldLabelModifier 
            '[UserDefined FirstLetterToLower, 
              UserDefined (StripConstructor TransactionToInitiator)]]
          TransactionToInitiator

deriveToSchemaFieldLabelModifier ''TransactionToInitiator [|modify (Proxy @TransactionToInitiator)|]