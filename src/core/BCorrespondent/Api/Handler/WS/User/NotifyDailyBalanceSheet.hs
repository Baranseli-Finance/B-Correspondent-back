{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module BCorrespondent.Api.Handler.WS.User.NotifyDailyBalanceSheet (handle) where

import BCorrespondent.Api.Handler.Frontend.User.InitDailyBalanceSheet (mkStatus)
import BCorrespondent.Auth (AuthenticatedUser (..), Role (..))
import BCorrespondent.Transport.Model.Frontend 
       (WSDashboardResource (WSDashboardResourceTimeline))
import BCorrespondent.Statement.Invoice (Status)
import BCorrespondent.Api.Handler.WS.Utils (withWS, ListenPsql, listenPsql, sendError, Resource (..), withResource)
import Katip.Handler (KatipHandlerM)
import qualified Network.WebSockets.Connection as WS
import Data.Text (Text)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.WithField (WithField)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Katip (Severity (DebugS), logTM)
import Data.String (fromString)
import BuildInfo (location)

data TimelineItem = 
     TimelineItem 
     { timelineItemDayOfYear :: Int,
       timelineItemHour :: Int,
       timelineItemMin :: Int,
       timelineItemTextualIdent :: Text
     }
    deriving stock (Generic)
     deriving
     (FromJSON, ToJSON)
     via WithOptions
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower, 
              UserDefined
              (StripConstructor TimelineItem)]]
          TimelineItem

type TimelineItemExt = Maybe (WithField "status" Status TimelineItem)

type instance ListenPsql "timeline_item_update" TimelineItemExt = ()

handle :: AuthenticatedUser 'Reader -> WS.Connection -> WSDashboardResource -> KatipHandlerM ()
handle AuthenticatedUser {institution = Nothing} conn WSDashboardResourceTimeline = 
  liftIO $ sendError conn "you haven't an institution assigned to to"
handle AuthenticatedUser {ident, institution = Just _} conn WSDashboardResourceTimeline =
  withWS @Resource conn $ \db resource -> do 
     $(logTM) DebugS $ fromString $ $location <> " received " <> show resource
     withResource @"Transaction" conn resource $ 
       listenPsql @"timeline_item_update" @TimelineItemExt conn db ident $ 
         fmap (first mkStatus)