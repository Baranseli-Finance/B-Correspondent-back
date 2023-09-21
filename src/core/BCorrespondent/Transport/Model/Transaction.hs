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
        Transaction
       ) where

import Data.UUID (UUID)
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier, modify)
import Data.Proxy (Proxy (..))
import Data.Swagger (ToSchema)
import Test.QuickCheck.Extended (Arbitrary)
import Database.Transaction (ParamsShow)

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
  deriving newtype (ToJSON, FromJSON, Arbitrary, ParamsShow)
 
instance ToSchema TransactionId

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