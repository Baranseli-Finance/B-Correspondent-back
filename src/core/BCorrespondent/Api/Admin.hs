{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Admin ( AdminApi (..) ) where

import BCorrespondent.Auth (JWT, AuthenticatedUser, Role (Admin))
import BCorrespondent.Transport.Response (Response)
import qualified Servant.Auth.Server as SA
import BCorrespondent.Transport.Model.Admin (NewUser)
import Servant.API.Extended
import Servant.API.Generic (Generic)

newtype AdminApi route = AdminApi
  { _adminApiCreateUser ::
       route
        :- Description "create new user"
          :> "user"
          :> SA.Auth '[JWT] (AuthenticatedUser 'Admin)
          :> ReqBody '[JSON] NewUser
          :> Put '[JSON] (Response ())
  }
  deriving stock (Generic)
