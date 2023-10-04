{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Frontend (FrontendApi (..), module Dashboard) where

import BCorrespondent.Api.Frontend.Dashboard as Dashboard (DashboardApi (..)) 
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Model.Frontend (Init) 
import BCorrespondent.Transport.Model.Auth (AuthToken)
import Servant.API.Extended
import Servant.API.Generic (Generic)

data FrontendApi route = FrontendApi
  { _frontendApiDashboard :: 
      route
        :- "dashboard"
          :> ToServant DashboardApi AsApi,
    _frontendApiInit ::
      route
        :- "init"
          :> QueryParam' '[Optional] "token" AuthToken
          :> Get '[JSON] (Response Init)
  }
  deriving stock (Generic)