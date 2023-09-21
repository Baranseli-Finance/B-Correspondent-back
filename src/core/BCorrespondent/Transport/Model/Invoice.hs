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
        InvoiceToElekse (..),
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

data Currency = USD | EUR
     deriving stock (Generic, Show)
     deriving (FromJSON, ToJSON)
      via WithOptions
          '[ConstructorTagModifier '[UserDefined ToLower]]
          Currency

mkArbitrary ''Currency

deriveToSchemaConstructorTag ''Currency [| map toLower |]

newtype ExternalInvoiceId = ExternalInvoiceId T.Text
  deriving stock (Generic, Show)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (Arbitrary)

instance ToSchema ExternalInvoiceId

newtype ExternalCustomerId = ExternalCustomerId T.Text
  deriving stock (Generic, Show)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (Arbitrary)

instance ToSchema ExternalCustomerId

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
    vat double required
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
       invoiceRegisterRequestVat :: !Double
     }
     deriving stock (Generic, Show)
     deriving (FromJSON, ToJSON)
       via WithOptions
          '[OmitNothingFields 'True, FieldLabelModifier '[UserDefined FirstLetterToLower, UserDefined (StripConstructor InvoiceRegisterRequest)]]
          InvoiceRegisterRequest

mkEncoder ''InvoiceRegisterRequest
mkArbitrary ''InvoiceRegisterRequest

encodeInvoice 
  :: InvoiceRegisterRequest
  -> (ExternalInvoiceId, ExternalCustomerId, Currency, UTCTime, T.Text,
      T.Text, Maybe T.Text, Maybe T.Text, T.Text, T.Text,
      Maybe T.Text, Maybe T.Text, T.Text, Double, Double)
encodeInvoice = fromMaybe undefined . mkEncoderInvoiceRegisterRequest

instance ParamsShow InvoiceRegisterRequest where
  render = show . fromMaybe undefined . mkEncoderInvoiceRegisterRequest

deriveToSchemaFieldLabelModifier ''InvoiceRegisterRequest [|firstLetterModify (Proxy @InvoiceRegisterRequest)|]

newtype InvoiceId = InvoiceId Int64
  deriving stock (Generic, Show)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (Arbitrary)

instance ToSchema InvoiceId

data InvoiceToElekse = InvoiceToElekse { invoiceToElekseExternalId :: UUID }
     deriving stock (Generic, Show)
     deriving (FromJSON, ToJSON)
       via WithOptions
          '[OmitNothingFields 'True, FieldLabelModifier '[UserDefined FirstLetterToLower, UserDefined (StripConstructor InvoiceToElekse)]]
          InvoiceToElekse