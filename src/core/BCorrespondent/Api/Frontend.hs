{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Frontend (FrontendApi (..)) where

import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Model.Frontend (ProcuratoryRequest, Init) 
import BCorrespondent.Auth (AuthenticatedUser, JWT)
import BCorrespondent.Transport.Model.Auth (AuthToken)
import Servant.API.Extended
import Servant.API.Generic (Generic)
import qualified Servant.Auth.Server as SA

data FrontendApi route = FrontendApi
  { _frontendApiGetHistory ::
      route
        :- "history"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> Get '[JSON] (Response ()),
    _frontendApiGetDayTimeline ::
      route
        :- "timeline"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> Get '[JSON] (Response ()),
    _frontendApiMakeProcuratory ::
      route
        :- "procuratory"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> ReqBody '[JSON] ProcuratoryRequest
          :> Put '[JSON] (Response ()),
    _frontendApiInit ::
      route
        :- "init"
          :> QueryParam' '[Optional] "token" AuthToken
          :> Get '[JSON] (Response Init)
  }
  deriving stock (Generic)
