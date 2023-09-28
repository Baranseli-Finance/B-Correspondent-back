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
    module Invoice,
    module Fs
  )
where

import BCorrespondent.Api.Foreign as Foreign
import BCorrespondent.Api.Auth as Auth
import BCorrespondent.Api.Frontend as Frontend
import BCorrespondent.Api.Invoice as Invoice
import  BCorrespondent.Api.Fs as Fs
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
    _httpApiInvoice ::
      route
        :- Tags "Invoice"
          :> "invoice"
          :> ToServant InvoiceApi AsApi,
    _httpApiFile ::
      route
        :- Tags "File"
          :> "file"
          :> ToServant FileApi AsApi 
  }
  deriving stock (Generic)
