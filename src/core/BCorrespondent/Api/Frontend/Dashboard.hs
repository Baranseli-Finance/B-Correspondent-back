{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}


module BCorrespondent.Api.Frontend.Dashboard (DashboardApi (..)) where

import BCorrespondent.Transport.Model.Frontend (DailyBalanceSheet)
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Model.Frontend (ProcuratoryRequest) 
import BCorrespondent.Auth (AuthenticatedUser, JWT, Role (..))
import Servant.API.Extended
import Servant.API.Generic (Generic)
import qualified Servant.Auth.Server as SA


data DashboardApi route = DashboardApi
  { _dashboardApiGetHistory ::
      route
        :- "history"
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> Get '[JSON] (Response ()),
    _dashboardApiInitDailyBalanceSheet ::
      route
        :- "daily-balance-sheet"
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> Get '[JSON] (Response DailyBalanceSheet),
    _dashboardApiMakeProcuratory ::
      route
        :- "procuratory"
          :> SA.Auth '[JWT] (AuthenticatedUser 'Writer)
          :> ReqBody '[JSON] ProcuratoryRequest
          :> Put '[JSON] (Response ())
  }
  deriving stock (Generic)
