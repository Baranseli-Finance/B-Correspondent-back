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

module BCorrespondent.Transport.Model.Frontend (ProcuratoryRequest (..)) where

import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics
import Data.Swagger.Schema.Extended 
       (deriveToSchemaFieldLabelModifier,
        firstLetterModify
       )
import Data.Proxy (Proxy (..))

data ProcuratoryRequest = 
     ProcuratoryRequest { procuratoryTest :: Text }
     deriving stock (Generic, Show)
     deriving (FromJSON, ToJSON)
       via WithOptions
          '[OmitNothingFields 'True, 
            FieldLabelModifier 
            '[UserDefined FirstLetterToLower, 
              UserDefined (StripConstructor ProcuratoryRequest)]]
          ProcuratoryRequest

deriveToSchemaFieldLabelModifier ''ProcuratoryRequest [|firstLetterModify (Proxy @ProcuratoryRequest)|]