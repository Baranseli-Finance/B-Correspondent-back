{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.User (AuthApi (..)) where

import BCorrespondent.Api.Controller.Auth.Password.MakeLink (ResetPasswordLink)
import BCorrespondent.Api.Controller.Auth.Password.Create (NewPassword)
import BCorrespondent.Auth (AuthenticatedUser, JWT)
import BCorrespondent.Transport.Model.User
import BCorrespondent.Transport.Response (Response)
import Servant.API.Extended
import Servant.API.Generic (Generic)
import qualified Servant.Auth.Server as SA
import Data.Int (Int64)
import RateLimit (KeyPolicy, RateLimit, FixedWindow, IPAddressPolicy)
import Data.Time.TypeLevel (TimePeriod (Second))

data AuthApi route = AuthApi
  { _authApiLogin ::
      route
        :- "login"
          :> Capture "auth_type" AuthType
          :> ReqBody '[JSON] Credentials
          :> Post '[JSON] (Response AuthToken),
    _authApiLogout ::
      route
        :- "logout"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> Post '[JSON] (Response ()),
    _authApiEmailLinkSend ::
      route
        :- "email"
          :> "link"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> Put '[JSON] (Response (Maybe Int)),
    _authApiResetPassMakeLink ::
      route
        :- "password"
          :> "reset"
          :> "link"
          :> RateLimit (FixedWindow (Second 1) 50) (KeyPolicy "Token")
          :> ReqBody '[JSON] ResetPasswordLink
          :> Put '[JSON] (Response (Maybe Int64)),
    _authApiResetPassNewPass ::
      route
        :- "password"
          :> "reset"
          :> RateLimit (FixedWindow (Second 1) 1) (IPAddressPolicy "fixed")
          :> ReqBody '[JSON] NewPassword
          :> Post '[JSON] (Response Bool) 
  }
  deriving stock (Generic)
