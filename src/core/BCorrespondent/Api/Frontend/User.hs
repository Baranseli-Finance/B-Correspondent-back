{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Frontend.User (UserApi (..)) where

import BCorrespondent.Transport.Model.Frontend 
       (DailyBalanceSheet, GapItem, WSDashboardResource, FetchGap)
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Model.Frontend 
       (ProcuratoryRequest, 
        GapItemTime, 
        TimelineDirection, 
        TimelineTransaction
       )
import BCorrespondent.Transport.Id (Id)       
import BCorrespondent.Auth (AuthenticatedUser, JWT, Role (..))
import Servant.API.Extended
import Servant.API.Generic (Generic)
import qualified Servant.Auth.Server as SA
import Servant.API.WebSocket (WebSocketPending)
import Servant.Swagger.Internal.Extended ()

data UserApi route = UserApi
  { _userApiGetHistory ::
      route
        :- "history"
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> Get '[JSON] (Response ()),
    _userApiInitDailyBalanceSheet ::
      route
        :- "dashboard"
          :> "daily-balance-sheet"
          :> "init"
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> Get '[JSON] (Response DailyBalanceSheet),
    _userApiFetchGap ::
      route
        :- "dashboard"
          :> "daily-balance-sheet"
          :> "gap"
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> QueryParam' '[Required, Strict] "from" GapItemTime
          :> QueryParam' '[Required, Strict] "to" GapItemTime
          :> Get '[JSON] (Response FetchGap),
    _userApiFetchOneHourTimeline ::
      route
        :- "dashboard"
          :> "daily-balance-sheet"
          :> "timeline"
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> Capture "direction" TimelineDirection
          :> QueryParam' '[Required, Strict] "point" GapItemTime
          :> Get '[JSON] (Response [GapItem]),      
    _userApiNotifyDailyBalanceSheet ::
      route
        :- "dashboard"
        :> "daily-balance-sheet"
        :> "notify"
        :> Capture "resource" WSDashboardResource
        :> WebSocketPending,
    _userApiMakeProcuratory ::
      route
        :- "procuratory"
          :> SA.Auth '[JWT] (AuthenticatedUser 'Writer)
          :> ReqBody '[JSON] ProcuratoryRequest
          :> Put '[JSON] (Response ()),
    _userApiGetTimelineTransaction ::
      route
        :- "dashboard"
          :> "timeline"
          :> "transaction"
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> Capture "ident" (Id "transaction")
          :> Get '[JSON] (Response TimelineTransaction)
  }
  deriving stock (Generic)
