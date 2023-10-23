{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Frontend.User (UserApi (..)) where

import BCorrespondent.Transport.Model.Frontend 
       (InitDashboard, GapItem, FetchGap, HistoryTimeline)
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Model.Frontend 
       (ProcuratoryRequest, 
        GapItemTime, 
        TimelineDirection, 
        TimelineTransactionResponse,
        HistoryDate,
        Notifications,
        Issue,
        BalancedBook
       )
import BCorrespondent.Transport.Id (Id)       
import BCorrespondent.Auth (AuthenticatedUser, JWT, Role (..))
import Servant.API.Extended
import Servant.API.Generic (Generic)
import qualified Servant.Auth.Server as SA
import Servant.API.WebSocket (WebSocketPending)
import Servant.Swagger.Internal.Extended ()
import RateLimit (RateLimit, FixedWindow, KeyPolicy)
import Data.Time.TypeLevel (TimePeriod (Second))

data UserApi route = UserApi
  { _userApiIntTimelineHistory ::
      route
        :- "history"
          :> "timeline"
          :> RateLimit (FixedWindow (Second 1) 50) (KeyPolicy "Token")
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> QueryParam' '[Required, Strict] "date" HistoryDate
          :> Get '[JSON] (Response HistoryTimeline),
    _userApiGetHourShiftTimelineHistory ::
      route
        :- "history"
          :> "timeline"
          :> RateLimit (FixedWindow (Second 1) 50) (KeyPolicy "Token")
          :> Capture "year" Int
          :> Capture "month" Int
          :> Capture "day" Int
          :> Capture "direction" TimelineDirection
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> QueryParam' '[Required, Strict] "hour" Int
          :> Get '[JSON] (Response [GapItem]),
    _userApiInitDashboard ::
      route
        :- "dashboard"
          :> "init"
          :> RateLimit (FixedWindow (Second 1) 50) (KeyPolicy "Token")
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> Get '[JSON] (Response InitDashboard),
    _userApiFetchGap ::
      route
        :- "dashboard"
          :> "timeline"
          :> "gap"
          :> RateLimit (FixedWindow (Second 1) 50) (KeyPolicy "Token")
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> QueryParam' '[Required, Strict] "from" GapItemTime
          :> QueryParam' '[Required, Strict] "to" GapItemTime
          :> Get '[JSON] (Response FetchGap),
    _userApiFetchOneHourTimeline ::
      route
        :- "dashboard"
          :> "timeline"
          :> RateLimit (FixedWindow (Second 1) 50) (KeyPolicy "Token")
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> Capture "direction" TimelineDirection
          :> QueryParam' '[Required, Strict] "point" GapItemTime
          :> Get '[JSON] (Response [GapItem]),      
    _userApiNotifyTransactionUpdate ::
      route
        :- "dashboard"
        :> "transaction"
        :> "update"
        :> WebSocketPending,
    _userApiNotifyWalletUpdate ::
      route
        :- "dashboard"
        :> "wallet"
        :> "update"
        :> WebSocketPending,        
    _userApiMakeProcuratory ::
      route
        :- "procuratory"
          :> RateLimit (FixedWindow (Second 1) 1) (KeyPolicy "Token")
          :> SA.Auth '[JWT] (AuthenticatedUser 'Writer)
          :> ReqBody '[JSON] ProcuratoryRequest
          :> Put '[JSON] (Response ()),
    _userApiGetTimelineTransaction ::
      route
        :- "dashboard"
          :> "timeline"
          :> "transaction"
          :> RateLimit (FixedWindow (Second 1) 50) (KeyPolicy "Token")
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> Capture "ident" (Id "transaction")
          :> Get '[JSON] (Response TimelineTransactionResponse),
    _userApiGetNotifications ::
      route
        :- "notifications"
          :> RateLimit (FixedWindow (Second 1) 50) (KeyPolicy "Token")
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> Get '[JSON] (Response Notifications),
    _userApiMarkNotificationRead ::
      route
        :- "notification"
          :> RateLimit (FixedWindow (Second 1) 100) (KeyPolicy "Token")
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> ReqBody '[JSON] (Id "NotificationIdent")
          :> Post '[JSON] (Response ()),          
    _userApiSubmitIssue ::
      route
        :- "issue"
          :> RateLimit (FixedWindow (Second 1) 1) (KeyPolicy "Token")
          :> SA.Auth '[JWT] (AuthenticatedUser 'None)
          :> ReqBody '[JSON] Issue
          :> Put '[JSON] (Response ()),
    _userApiInitBalancedBook ::
      route
        :- "balanced-book"
          :> RateLimit (FixedWindow (Second 1) 50) (KeyPolicy "Token")
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> Get '[JSON] (Response BalancedBook)
  }
  deriving stock (Generic)
