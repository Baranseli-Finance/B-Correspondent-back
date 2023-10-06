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
{-# OPTIONS_GHC -fno-warn-missing-exported-signatures #-}

module BCorrespondent.Transport.Model.Invoice 
       (Currency (..), 
        ExternalInvoiceId (..), 
        InvoiceRegisterRequest (..), 
        InvoiceId (..),
        ExternalCustomerId (..),
        InvoiceToPaymentProvider (..),
        InvoiceRegisterResponse (..),
        Fee (..),
        encodeInvoice
       ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics
import Data.Swagger.Schema.Extended 
       (deriveToSchemaFieldLabelModifier, 
        deriveToSchemaConstructorTag,
        firstLetterModify
       )
import Data.Proxy (Proxy (..))
import Data.Swagger (ToSchema)
import TH.Mk (mkArbitrary, mkEncoder)
import Test.QuickCheck.Extended (Arbitrary)
import Data.Char (toLower)
import qualified Data.Text as T (Text)
import Database.Transaction (ParamsShow (..))
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import Data.Int (Int64)
import Data.UUID (UUID)
import Data.Tuple.Extended (del17)
import Data.Coerce (coerce)
import Data.String.Conv (toS)

data Currency = USD | EUR
     deriving stock (Generic, Show, Eq, Read)
     deriving (FromJSON, ToJSON)
      via WithOptions
          '[ConstructorTagModifier 
            '[UserDefined ToLower]]
          Currency

mkArbitrary ''Currency

deriveToSchemaConstructorTag ''Currency [| map toLower |]

newtype ExternalInvoiceId = ExternalInvoiceId T.Text
  deriving stock (Generic, Show, Eq)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (Arbitrary)

instance ToSchema ExternalInvoiceId

newtype ExternalCustomerId = ExternalCustomerId T.Text
  deriving stock (Generic, Show, Eq)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (Arbitrary)

instance ToSchema ExternalCustomerId


-- :71A::Details of charges
-- Use this three-letter field to specify the party that will pay the transaction charges. The possible values are:
-- OUR for charges to be paid by the ordering customer; or
-- SHA for charges to be shared by the ordering customer and the final beneficiary.
data Fee = OUR | SHA
     deriving stock (Generic, Show, Eq, Read)
     deriving (FromJSON, ToJSON)
      via WithOptions
          '[ConstructorTagModifier 
            '[UserDefined ToLower]]
          Fee

mkArbitrary ''Fee

deriveToSchemaConstructorTag ''Fee [| map toLower |]

{-
invoice:   
    customer id text required,
    invoice id text required,
    invoice time utc time required ,
    seller text required,
    seller address text required,
    seller tax id text optional,
    seller phone number optional,
    buyer text required,
    buyer address text required,
    buyer tax id text optional,
    buyer phone number text optional,
    payment description text required,
    currency text required,
    amount double required,
    vat double required,
    fee enum Fee required
-}
data InvoiceRegisterRequest =
     InvoiceRegisterRequest 
     { invoiceRegisterRequestInvoiceIdent :: !ExternalInvoiceId,
       invoiceRegisterRequestCustomerIdent :: !ExternalCustomerId,
       invoiceRegisterRequestCurrency :: !Currency,
       invoiceRegisterRequestCreatedAt :: !UTCTime,
       invoiceRegisterRequestSeller :: !T.Text,
       invoiceRegisterRequestSellerAddress :: !T.Text,
       invoiceRegisterRequestSellerTaxId :: !(Maybe T.Text),
       invoiceRegisterRequestSellerPhoneNumber :: !(Maybe T.Text),
       invoiceRegisterRequestBuyer :: !T.Text,
       invoiceRegisterRequestBuyerAddress :: !T.Text,
       invoiceRegisterRequestBuyerTaxId :: !(Maybe T.Text),
       invoiceRegisterRequestBuyerPhoneNumber :: !(Maybe T.Text),
       invoiceRegisterRequestPaymentDescription :: !T.Text,
       invoiceRegisterRequestAmount :: !Double,
       invoiceRegisterRequestVat :: !Double,
       invoiceRegisterRequestFee :: !Fee,
       invoiceRegisterRequestCountryISOCode :: !T.Text
     }
     deriving stock (Generic, Show, Eq)
     deriving (FromJSON, ToJSON)
       via WithOptions
          '[OmitNothingFields 'True, 
            FieldLabelModifier 
            '[UserDefined FirstLetterToLower, 
              UserDefined (StripConstructor InvoiceRegisterRequest)]]
          InvoiceRegisterRequest

mkEncoder ''InvoiceRegisterRequest
mkArbitrary ''InvoiceRegisterRequest

encodeInvoice 
  :: InvoiceRegisterRequest
  -> (ExternalInvoiceId, ExternalCustomerId, Currency, UTCTime, T.Text,
      T.Text, Maybe T.Text, Maybe T.Text, T.Text, T.Text,
      Maybe T.Text, Maybe T.Text, T.Text, Double, Double, Fee)
encodeInvoice = fromMaybe undefined . fmap del17 . mkEncoderInvoiceRegisterRequest

instance ParamsShow InvoiceRegisterRequest where
  render InvoiceRegisterRequest {..} = 
    render @T.Text (coerce invoiceRegisterRequestInvoiceIdent) <> ", " <>
    render @T.Text (coerce invoiceRegisterRequestCustomerIdent) <> ", " <>
    render @T.Text (toS (show invoiceRegisterRequestCurrency)) <> ", " <>
    render invoiceRegisterRequestCreatedAt <> ", " <>
    render invoiceRegisterRequestSeller <> ", " <>
    render invoiceRegisterRequestSellerAddress <> ", " <>
    render invoiceRegisterRequestSellerTaxId <> ", " <>
    render invoiceRegisterRequestSellerPhoneNumber <> ", " <> 
    render invoiceRegisterRequestBuyer <> ", " <>
    render invoiceRegisterRequestBuyerAddress <> ", " <> 
    render invoiceRegisterRequestBuyerTaxId <> ", " <>
    render invoiceRegisterRequestBuyerPhoneNumber <> ", " <>
    render invoiceRegisterRequestPaymentDescription <> ", " <>
    render invoiceRegisterRequestAmount <> ", " <>
    render invoiceRegisterRequestVat <> ", " <>
    render @T.Text (toS (show invoiceRegisterRequestFee))

deriveToSchemaFieldLabelModifier ''InvoiceRegisterRequest [|firstLetterModify (Proxy @InvoiceRegisterRequest)|]

newtype InvoiceId = InvoiceId Int64
  deriving stock (Generic, Show)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (Arbitrary)

instance ToSchema InvoiceId

data InvoiceRegisterResponse =
     InvoiceRegisterResponse 
     { invoiceRegisterResponseExternalIdent :: !ExternalInvoiceId }
     deriving stock (Generic, Show)
     deriving (FromJSON, ToJSON)
       via WithOptions
          '[OmitNothingFields 'True, 
            FieldLabelModifier 
            '[UserDefined FirstLetterToLower, 
              UserDefined (StripConstructor InvoiceRegisterResponse)]]
          InvoiceRegisterResponse

deriveToSchemaFieldLabelModifier 
  ''InvoiceRegisterResponse 
  [|firstLetterModify (Proxy @InvoiceRegisterResponse)|]

data InvoiceToPaymentProvider = 
     InvoiceToPaymentProvider 
     { invoiceToPaymentProviderExternalId :: UUID }
     deriving stock (Generic, Show)
     deriving (FromJSON, ToJSON)
       via WithOptions
          '[OmitNothingFields 'True, 
            FieldLabelModifier 
            '[UserDefined FirstLetterToLower, 
              UserDefined (StripConstructor InvoiceToPaymentProvider)]]
          InvoiceToPaymentProvider