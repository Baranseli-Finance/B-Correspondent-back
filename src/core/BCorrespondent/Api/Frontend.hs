{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Frontend (FrontendApi (..), module User) where

import BCorrespondent.Api.Frontend.User as User (UserApi (..)) 
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Model.Frontend (Init) 
import BCorrespondent.Transport.Model.Auth (AuthToken)
import Servant.API.Extended
import Servant.API.Generic (Generic)

data FrontendApi route = FrontendApi
  { _frontendApiUser :: 
      route
        :- "user"
          :> ToServant UserApi AsApi,
    _frontendApiInit ::
      route
        :- "init"
          :> QueryParam' '[Optional] "token" AuthToken
          :> Get '[JSON] (Response Init)
  }
  deriving stock (Generic)