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
        invoiceSince,
        Sha (..),
        JWTStatus (..),
        LogLevel,
        GapItem (..),
        GapItemUnit (..),
        GapItemUnitStatus (..),
        DailyBalanceSheet (..),
        GapItemTime (..),
        TimelineDirection (..),
        FetchGap (..),
        TimelineTransaction (..),
        TimelineTransactionResponse (..),
        InitDashboard (..),
        Wallet (..),
        WalletType (..),
        GapItemAmount (..),
        InvoiceSince (..),
        -- * history endpoint
        HistoryDate (..),
        HistoryTimeline (..),
        Notifications (..),
        Notification,
        Issue,
        encodeHistoryDate
       ) where

import BCorrespondent.Transport.Model.Invoice (Currency)
import Data.Text (Text, splitOn, unpack)
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)
import Data.Swagger.Schema.Extended 
       (deriveToSchemaFieldLabelModifier,
        firstLetterModify,
        deriveToSchemaConstructorTag,
        deriveToSchema
       )
import Data.Proxy (Proxy (..))
import Data.Default.Class
import Data.Default.Class.Extended ()
import TH.Mk 
       (mkToSchemaAndJSON, 
        mkEnumConvertor, 
        mkParamSchemaEnum, 
        mkFromHttpApiDataEnum, 
        mkArbitrary,
        mkEncoder
       )
import Control.Lens
import Control.Lens.Iso.Extended (jsonb, stext)
import Data.Char (toLower)
import Data.Swagger
import Servant.API (FromHttpApiData (..))
import Data.Int (Int64)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Data.Aeson.TH.Extended (deriveToJSON, defaultOptions)
import Data.Maybe (fromMaybe)
import Database.Transaction (ParamsShow (..))


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

data InvoiceSince =
     InvoiceSince 
     { invoiceSinceYear :: !Int,
       invoiceSinceMonth :: !Int,
       invoiceSinceDay :: !Int
     } 
    deriving stock (Generic, Show)
    deriving
      (FromJSON, ToJSON)
      via WithOptions 
         '[FieldLabelModifier 
           '[UserDefined ToLower,
             UserDefined (StripConstructor InvoiceSince)]]
      InvoiceSince

instance Default InvoiceSince

deriveToSchemaFieldLabelModifier ''InvoiceSince [| firstLetterModify (Proxy @InvoiceSince) |]

data Init =
     Init
    { shaXs :: ![Sha],
      isJwtValid :: !JWTStatus,
      level :: !LogLevel,
      toTelegram :: !Bool,
      telegramBot :: !Text,
      telegramChat :: !Text,
      loadCssLocally :: !Bool,
      invoiceSince :: !InvoiceSince 
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

defInit = def @Init

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
     | Ok 
     | Declined
     deriving stock (Generic, Show)

mkToSchemaAndJSON ''GapItemUnitStatus
mkEnumConvertor ''GapItemUnitStatus
mkParamSchemaEnum ''GapItemUnitStatus [|isoGapItemUnitStatus . jsonb|]
mkFromHttpApiDataEnum ''GapItemUnitStatus [|from stext . from isoGapItemUnitStatus . to Right|]

data GapItemUnit =
     GapItemUnit 
     { gapItemUnitIdent :: Int64,
       gapItemUnitTm :: UTCTime,
       gapItemUnitTextualIdent :: Text,
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

data GapItemAmount = 
     GapItemAmount 
     { gapItemAmountCurrency :: Currency, 
       gapItemAmountValue :: Double 
     }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructor 
               GapItemAmount)]]
      GapItemAmount

deriveToSchemaFieldLabelModifier ''GapItemAmount [|firstLetterModify (Proxy @GapItemAmount)|]

data GapItem =
     GapItem
     { gapItemStart :: !GapItemTime,
       gapItemEnd :: !GapItemTime,
       gapItemElements :: ![GapItemUnit],
       gapItemAmounts :: ![GapItemAmount]
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
     { dailyBalanceSheetInstitution :: !Text,
       dailyBalanceSheetGaps :: ![GapItem] 
     }
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

data WalletType = Debit | Credit
     deriving stock (Generic, Show)
     deriving
     (ToJSON, FromJSON)
     via WithOptions
          '[ConstructorTagModifier 
            '[UserDefined ToLower]]
         WalletType

deriveToSchemaConstructorTag ''WalletType [| map toLower |]

data Wallet = 
     Wallet 
     { walletIdent :: !Int64,
       walletCurrency :: !Currency,
       walletAmount :: !Double,
       walletWalletType :: !WalletType
     }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructor 
               Wallet)]]
      Wallet

deriveToSchemaFieldLabelModifier ''Wallet [|firstLetterModify (Proxy @Wallet)|]

data InitDashboard = 
     InitDashboard 
     { initDashboardDailyBalanceSheet :: !DailyBalanceSheet,
       initDashboardWallets :: ![Wallet]
     }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructor 
               InitDashboard)]]
      InitDashboard

deriveToSchemaFieldLabelModifier ''InitDashboard [|firstLetterModify (Proxy @InitDashboard)|]

data TimelineDirection = Backward | Forward
  deriving stock (Generic, Show, Eq)
  deriving (Enum)

mkToSchemaAndJSON ''TimelineDirection
mkEnumConvertor ''TimelineDirection
mkParamSchemaEnum ''TimelineDirection [|isoTimelineDirection . jsonb|]
mkFromHttpApiDataEnum ''TimelineDirection [|from stext . from isoTimelineDirection . to Right|]

data FetchGap = FetchGap { fetchGapGap :: GapItem }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructor 
               FetchGap)]]
      FetchGap

deriveToSchemaFieldLabelModifier ''FetchGap [|firstLetterModify (Proxy @FetchGap)|]


data TimelineTransaction =
     TimelineTransaction
     {
       timelineTransactionIdent :: UUID,
       timelineTransactionSenderName :: Text,
       timelineTransactionSenderAddress :: Text,
       timelineTransactionSenderPhoneNumber :: Text,
       timelineTransactionSenderBank :: Text,
       timelineTransactionSwiftSepaCode :: Text,
       timelineTransactionSenderBankAccount :: Text,
       timelineTransactionAmount :: Double,
       timelineTransactionCurrency :: Currency,
       timelineTransactionCorrespondentBank :: Text,
       timelineTransactionCorrespondentBankSwiftSepaCode :: Text
     }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructor 
               TimelineTransaction)]]
      TimelineTransaction

deriveToSchemaFieldLabelModifier 
  ''TimelineTransaction 
  [|firstLetterModify (Proxy @TimelineTransaction)|]

data TimelineTransactionResponse =
     TimelineTransactionResponse
     { transaction :: TimelineTransaction }
    deriving stock (Generic, Show)

deriveToJSON defaultOptions ''TimelineTransactionResponse

deriveToSchema ''TimelineTransactionResponse

-- history 

data HistoryDate = 
     HistoryDate 
     { historyDateYear :: Int,
       historyDateMonth :: Int,
       historyDateDay :: Int
     }
    deriving stock (Generic, Show, Ord, Eq)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructor 
               HistoryDate)]]
      HistoryDate

instance ToParamSchema HistoryDate where
  toParamSchema _ = mempty & type_ ?~ SwaggerString 

instance FromHttpApiData HistoryDate where
  parseUrlPiece s = 
    case map (read @Int . unpack) (splitOn "," s) of
      [y, m, d] -> Right $ HistoryDate y m d
      _ -> Left $ "cannot parse " <> s <> " into HistoryDate"

mkEncoder ''HistoryDate
mkArbitrary ''HistoryDate

encodeHistoryDate :: HistoryDate -> (Int, Int, Int)
encodeHistoryDate = fromMaybe undefined . mkEncoderHistoryDate

instance ParamsShow HistoryDate where
  render = show . encodeHistoryDate

data HistoryTimeline = 
     HistoryTimeline 
     { historyTimelineInstitution :: Text,
       historyTimelineTimeline :: [GapItem] 
     }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructor 
               HistoryTimeline)]]
      HistoryTimeline

deriveToSchemaFieldLabelModifier ''HistoryTimeline [|firstLetterModify (Proxy @HistoryTimeline)|]

data Notification = Notification { notificationText :: Text }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructor 
               Notification)]]
      Notification

deriveToSchemaFieldLabelModifier ''Notification [|firstLetterModify (Proxy @Notification)|]

data Notifications = 
     Notifications
     { notificationsCount :: Int,
       notificationsItems :: [Notification]
     }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructor 
               Notifications)]]
      Notifications

deriveToSchemaFieldLabelModifier ''Notifications [|firstLetterModify (Proxy @Notifications)|]

data Issue = 
     Issue 
     { issueDescription :: !Text,
       issueFiles :: ![Int64]
     }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructor 
               Issue)]]
      Issue

deriveToSchemaFieldLabelModifier ''Issue [|firstLetterModify (Proxy @Issue)|]