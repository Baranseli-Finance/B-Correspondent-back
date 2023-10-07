{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}


module BCorrespondent.Api.Frontend.Dashboard (DashboardApi (..)) where

import BCorrespondent.Transport.Model.Frontend 
       (DailyBalanceSheet, GapItem, WSResource)
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Model.Frontend (ProcuratoryRequest, GapItemTime)
import BCorrespondent.Auth (AuthenticatedUser, JWT, Role (..))
import Servant.API.Extended
import Servant.API.Generic (Generic)
import qualified Servant.Auth.Server as SA
import Servant.API.WebSocket (WebSocketPending)
import Servant.Swagger.Internal.Extended ()

data DashboardApi route = DashboardApi
  { _dashboardApiGetHistory ::
      route
        :- "history"
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> Get '[JSON] (Response ()),
    _dashboardApiInitDailyBalanceSheet ::
      route
        :- "daily-balance-sheet"
          :> "init"
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> Get '[JSON] (Response DailyBalanceSheet),
    _dashboardApiFetchGap ::
      route
        :- "daily-balance-sheet"
          :> "gap"
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> QueryParam' '[Required, Strict] "from" GapItemTime
          :> QueryParam' '[Required, Strict] "to" GapItemTime
          :> Get '[JSON] (Response GapItem),
    _dashboardApiNotifyDailyBalanceSheet ::
      route
        :- "daily-balance-sheet"
        :> "notify"
        :> Capture "resource" WSResource
        :> WebSocketPending,
    _dashboardApiMakeProcuratory ::
      route
        :- "procuratory"
          :> SA.Auth '[JWT] (AuthenticatedUser 'Writer)
          :> ReqBody '[JSON] ProcuratoryRequest
          :> Put '[JSON] (Response ())
  }
  deriving stock (Generic)
