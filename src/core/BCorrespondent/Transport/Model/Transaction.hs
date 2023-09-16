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

module BCorrespondent.Transport.Model.Transaction 
       (TransactionConfirmed, 
        TransactionId,
        TransactionNewRequest    
       ) where

import Data.UUID (UUID)
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier, modify)
import Data.Proxy (Proxy (..))
import Data.Swagger (ToSchema)

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
  deriving newtype (ToJSON)
 
instance ToSchema TransactionId

data TransactionNewRequest = 
     TransactionNewRequest 
     { transactionNewRequestIdent :: Int }
     deriving stock (Generic, Show)
     deriving (FromJSON)
       via WithOptions
          '[OmitNothingFields 'True, FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor TransactionNewRequest)]]
          TransactionNewRequest

deriveToSchemaFieldLabelModifier ''TransactionNewRequest [|modify (Proxy @TransactionNewRequest)|]