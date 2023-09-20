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

module BCorrespondent.Transport.Model.Invoice (Currency (..), ExternalInvoiceId (..), InvoiceRegisterRequest (..), InvoiceId) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics
import Data.Swagger.Schema.Extended 
       (deriveToSchemaFieldLabelModifier, 
        deriveToSchemaConstructorTag, 
        modify
       )
import Data.Proxy (Proxy (..))
import Data.Swagger (ToSchema)
import TH.Mk (mkArbitrary, mkEncoder)
import Test.QuickCheck.Extended (Arbitrary)
import Data.Char (toLower)
import Data.Word (Word64)

data Currency = USD | EU
     deriving stock (Generic, Show)
     deriving (FromJSON, ToJSON)
      via WithOptions
          '[ConstructorTagModifier '[UserDefined ToLower]]
          Currency

mkArbitrary ''Currency

deriveToSchemaConstructorTag ''Currency [| map toLower |]

newtype ExternalInvoiceId = ExternalInvoiceId Word64
  deriving stock (Generic, Show)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (Arbitrary)

instance ToSchema ExternalInvoiceId

data InvoiceRegisterRequest = 
     InvoiceRegisterRequest 
     { invoiceRegisterRequestIdent :: !ExternalInvoiceId,
       invoiceRegisterRequestCurrency :: !Currency,
       invoiceRegisterRequestAmount :: !Double
     }
     deriving stock (Generic, Show)
     deriving (FromJSON, ToJSON)
       via WithOptions
          '[OmitNothingFields 'True, FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor InvoiceRegisterRequest)]]
          InvoiceRegisterRequest

mkEncoder ''InvoiceRegisterRequest
mkArbitrary ''InvoiceRegisterRequest

deriveToSchemaFieldLabelModifier ''InvoiceRegisterRequest [|modify (Proxy @InvoiceRegisterRequest)|]

newtype InvoiceId = InvoiceId Word64
  deriving stock (Generic, Show)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (Arbitrary)

instance ToSchema InvoiceId