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
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-exported-signatures #-}

module BCorrespondent.Transport.Model.Frontend 
       (ProcuratoryRequest (..),
        Init,
        defInit,
        isJwtValid,
        shaXs,
        toTelegram,
        telegramBot,
        telegramChat,
        level,
        loadCssLocally,
        Sha (..),
        JWTStatus (..),
        LogLevel,
        GapItem (..),
        GapItemUnit (..),
        GapItemUnitStatus (..),
        DailyBalanceSheet (..),
        GapItemTime (..),
        WSDashboardResource (..)
       ) where

import Data.Text (Text, splitOn, unpack)
import Data.Aeson (ToJSON, FromJSON, Value (String))
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)
import Data.Swagger.Schema.Extended 
       (deriveToSchemaFieldLabelModifier,
        firstLetterModify,
        deriveToSchemaConstructorTag,
        modify
       )
import Data.Proxy (Proxy (..))
import Data.Default.Class
import Data.Default.Class.Extended ()
import TH.Mk 
       (mkToSchemaAndJSON, 
        mkEnumConvertor, 
        mkParamSchemaEnum, 
        mkFromHttpApiDataEnum, 
        mkArbitrary
       )
import Control.Lens
import Control.Lens.Iso.Extended (jsonb, stext)
import Data.Char (toLower)
import Data.Swagger
import Servant.API (FromHttpApiData (..))

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

deriveToSchemaFieldLabelModifier 
  ''ProcuratoryRequest 
  [|firstLetterModify (Proxy @ProcuratoryRequest)|]

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


data LogLevel = Prod | Dev
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON)
    via WithOptions
       '[ConstructorTagModifier '[UserDefined ToLower]]
    LogLevel

instance Default LogLevel where
  def = Dev

mkArbitrary ''LogLevel

deriveToSchemaConstructorTag ''LogLevel [| map toLower |]

data Init =
     Init
    { shaXs :: ![Sha],
      isJwtValid :: !JWTStatus,
      level :: !LogLevel,
      toTelegram :: !Bool,
      telegramBot :: !Text,
      telegramChat :: !Text,
      loadCssLocally :: !Bool
    }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
         '[FieldLabelModifier 
           '[UserDefined ToLower]] 
      Init

instance Default Init

deriveToSchemaFieldLabelModifier ''Init [| map toLower |]

defInit = Init def def def def def def def

-- daily balance sheet

data GapItemTime = 
      GapItemTime 
      { gapItemTimeHour :: Int, 
        gapItemTimeMin :: Int
      }
    deriving stock (Generic, Show, Ord, Eq)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructor 
               GapItemTime)]]
      GapItemTime

deriveToSchemaFieldLabelModifier ''GapItemTime [|firstLetterModify (Proxy @GapItemTime)|]

instance ToParamSchema GapItemTime where
  toParamSchema _ = mempty & type_ ?~ SwaggerString 

instance FromHttpApiData GapItemTime where
  parseUrlPiece s = 
    case map (read @Int . unpack) (splitOn "," s) of
      [h, m] -> Right $ GapItemTime h m
      _ -> Left $ "cannot parse " <> s <> " into GapItemTime"

data GapItemUnitStatus =
       Pending
     | ProcessedOk 
     | ProcessedDecline
     deriving stock (Generic, Show)

mkToSchemaAndJSON ''GapItemUnitStatus
mkEnumConvertor ''GapItemUnitStatus
mkParamSchemaEnum ''GapItemUnitStatus [|isoGapItemUnitStatus . jsonb|]
mkFromHttpApiDataEnum ''GapItemUnitStatus [|from stext . from isoGapItemUnitStatus . to Right|]

data GapItemUnit =
     GapItemUnit 
     { gapItemUnitTextualIdent :: Text, 
       gapItemUnitStatus :: GapItemUnitStatus
     }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructor 
               GapItemUnit)]]
      GapItemUnit

deriveToSchemaFieldLabelModifier ''GapItemUnit [|firstLetterModify (Proxy @GapItemUnit)|]

data GapItem =
     GapItem
     { gapItemStart :: !GapItemTime,
       gapItemEnd :: !GapItemTime,
       gapItemElements :: ![GapItemUnit]
     }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructor 
               GapItem)]]
      GapItem

deriveToSchemaFieldLabelModifier ''GapItem [|firstLetterModify (Proxy @GapItem)|]

data DailyBalanceSheet = 
     DailyBalanceSheet
     { dailyBalanceSheetGaps :: ![GapItem] }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructor 
               DailyBalanceSheet)]]
      DailyBalanceSheet

deriveToSchemaFieldLabelModifier ''DailyBalanceSheet [|firstLetterModify (Proxy @DailyBalanceSheet)|]

data WSDashboardResource = WSDashboardResourceTimeline deriving (Show)

mkEnumConvertor ''WSDashboardResource
mkParamSchemaEnum ''WSDashboardResource [|isoWSDashboardResource . to (modify (Proxy @WSDashboardResource)) . stext . to String|]
mkFromHttpApiDataEnum ''WSDashboardResource [|from stext . from isoWSDashboardResource . to Right|]