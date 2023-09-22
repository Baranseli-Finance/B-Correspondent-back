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

module BCorrespondent.Transport.Model.Frontend 
       (ProcuratoryRequest (..),
        Init,
        defInit,
        isJwtValid,
        shaXs,
        Sha (..),
        JWTStatus (..)
       ) where

import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)
import Data.Swagger.Schema.Extended 
       (deriveToSchemaFieldLabelModifier,
        firstLetterModify
       )
import Data.Proxy (Proxy (..))
import Data.Default.Class
import Data.Default.Class.Extended ()
import TH.Mk (mkToSchemaAndJSON, mkEnumConvertor, mkParamSchemaEnum, mkFromHttpApiDataEnum)
import Control.Lens
import Control.Lens.Iso.Extended (jsonb, stext)
import Data.Char (toLower)


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

data JWTStatus = Valid | Invalid | Skip
  deriving stock (Generic, Show)
  deriving (Enum)

instance Default JWTStatus where
  def = Valid

mkToSchemaAndJSON ''JWTStatus
mkEnumConvertor ''JWTStatus
mkParamSchemaEnum ''JWTStatus [|isoJWTStatus . jsonb|]
mkFromHttpApiDataEnum ''JWTStatus [|from stext . from isoJWTStatus . to Right|]

data Sha = Sha { shaKey :: !Text, shaValue :: !Text } 
     deriving stock (Generic, Show)
     deriving (FromJSON, ToJSON)
       via WithOptions
          '[OmitNothingFields 'True, 
            FieldLabelModifier 
            '[UserDefined FirstLetterToLower, 
              UserDefined (StripConstructor Sha)]]
          Sha

deriveToSchemaFieldLabelModifier ''Sha [|firstLetterModify (Proxy @Sha)|]

data Init =
     Init
    { shaXs :: ![Sha],
      isJwtValid :: !JWTStatus
    }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
         '[FieldLabelModifier 
           '[UserDefined ToLower]] Init

instance Default Init

deriveToSchemaFieldLabelModifier ''Init [| map toLower |]

defInit = Init def def