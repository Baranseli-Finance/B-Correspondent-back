{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Map
  ( HttpApi (..),
    module Auth,
    module Foreign,
    module Frontend,
    module Institution,
    module Fs,
    module Admin,
    module WS
  )
where

import BCorrespondent.Api.Foreign as Foreign
import BCorrespondent.Api.Auth as Auth
import BCorrespondent.Api.Frontend as Frontend
import BCorrespondent.Api.Institution as Institution  
import BCorrespondent.Api.Fs as Fs
import BCorrespondent.Api.Admin as Admin
import BCorrespondent.Api.WS as WS
import Servant.API
import Servant.API.Generic
import Servant.Swagger.Tags

data HttpApi route = HttpApi
  { _httpApiAuth ::
      route
        :- Tags "Auth"
          :> "auth"
          :> ToServant AuthApi AsApi,
    _httpApiForeign ::
      route
        :- Tags "Foreign"
          :> "foreign"
          :> ToServant ForeignApi AsApi,
    _httpApiFrontend ::
      route
        :- Tags "Frontend"
          :> "frontend"
          :> ToServant FrontendApi AsApi,
    _httpApiInstitution ::
      route
        :- Tags "Institution"
          :> "institution"
          :> ToServant InstitutionApi AsApi,
    _httpApiFile ::
      route
        :- Tags "File"
          :> "file"
          :> ToServant FileApi AsApi,
    _httpApiAdmin ::
      route
        :- Tags "Admin"
          :> "admin"
          :> ToServant AdminApi AsApi,
    _wsApi ::
      route
        :- Tags "WebSocket"
          :> "ws"
          :> ToServant WSApi AsApi

  }
  deriving stock (Generic)
