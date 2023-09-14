{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Buzgibi.Api.Map
  ( HttpApi (..),
    module File,
    module User,
    module Foreign
  )
where

import Buzgibi.Api.File as File
import Buzgibi.Api.Foreign as Foreign
import Buzgibi.Api.User as User
import Servant.API
import Servant.API.Generic
import Servant.Swagger.Tags

data HttpApi route = HttpApi
  { _httpApiFile ::
      route
        :- Tags "File"
          :> "file"
          :> ToServant FileApi AsApi,
    _httpApiAuth ::
      route
        :- Tags "Auth"
          :> "auth"
          :> ToServant AuthApi AsApi,
    _httpApiForeign ::
      route
        :- Tags "Foreign"
          :> "foreign"
          :> ToServant ForeignApi AsApi
  }
  deriving stock (Generic)
