{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Frontend.User (UserApi (..)) where

import BCorrespondent.Transport.Model.Frontend 
       (InitDashboard, GapItem, FetchGap)
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Model.Frontend 
       (ProcuratoryRequest, 
        GapItemTime, 
        TimelineDirection, 
        TimelineTransactionResponse
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
    _userApiInitDashboard ::
      route
        :- "dashboard"
          :> "init"
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> Get '[JSON] (Response InitDashboard),
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
    _userApiNotifyTransactionUpdate ::
      route
        :- "dashboard"
        :> "daily-balance-sheet"
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
          :> Get '[JSON] (Response TimelineTransactionResponse)
  }
  deriving stock (Generic)
