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
       (TransactionConfirmed, 
        TransactionId,
        TransactionRegisterRequest,
        Transaction,
        Currency (..),
        ExternalTransactionIdent
       ) where

import Data.UUID (UUID)
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

data TransactionConfirmed =
     TransactionConfirmed 
     { transactionConfirmedIdent :: UUID }
     deriving stock (Generic, Show)
     deriving (ToJSON)
       via WithOptions
          '[OmitNothingFields 'True, FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor TransactionConfirmed)]]
          TransactionConfirmed

deriveToSchemaFieldLabelModifier ''TransactionConfirmed [|modify (Proxy @TransactionConfirmed)|]


newtype TransactionId = TransactionId UUID
  deriving stock (Generic)
  deriving newtype (ToJSON, FromJSON)
 
instance ToSchema TransactionId

data Currency = USD | EU
     deriving stock (Generic, Show)
     deriving (FromJSON, ToJSON)
      via WithOptions
          '[ConstructorTagModifier '[UserDefined ToLower]]
          Currency

mkArbitrary ''Currency

deriveToSchemaConstructorTag ''Currency [| map toLower |]

newtype ExternalTransactionIdent = ExternalTransactionIdent Word64
  deriving stock (Generic, Show)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (Arbitrary)

instance ToSchema ExternalTransactionIdent

data TransactionRegisterRequest = 
     TransactionRegisterRequest 
     { transactionRegisterRequestIdent :: !ExternalTransactionIdent,
       transactionRegisterRequestCurrency :: !Currency,
       transactionRegisterRequestAmount :: !Double
     }
     deriving stock (Generic, Show)
     deriving (FromJSON, ToJSON)
       via WithOptions
          '[OmitNothingFields 'True, FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor TransactionRegisterRequest)]]
          TransactionRegisterRequest

mkEncoder ''TransactionRegisterRequest
mkArbitrary ''TransactionRegisterRequest

deriveToSchemaFieldLabelModifier ''TransactionRegisterRequest [|modify (Proxy @TransactionRegisterRequest)|]

data Transaction = 
     Transaction 
     { transactionIdent :: UUID
     }
     deriving stock (Generic, Show)
     deriving (ToJSON)
       via WithOptions
          '[OmitNothingFields 'True, FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Transaction)]]
          Transaction

deriveToSchemaFieldLabelModifier ''Transaction [|modify (Proxy @Transaction)|]