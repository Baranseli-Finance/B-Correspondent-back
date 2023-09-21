{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Frontend (FrontendApi (..)) where

import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Model.Frontend (ProcuratoryRequest) 
import BCorrespondent.Auth (AuthenticatedUser, JWT)
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
          :> Put '[JSON] (Response ())
  }
  deriving stock (Generic)
