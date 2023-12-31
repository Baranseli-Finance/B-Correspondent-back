{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Auth (AuthApi (..)) where

import BCorrespondent.Transport.Model.Auth 
       (AuthToken, InstitutionKey, NewPassword, AuthType, Credentials, AuthCode, AuthCodeHash, ResendCode)
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Auth (AuthenticatedUser, JWT, Role (..))
import Servant.API.Extended
import Servant.API.Generic (Generic)
import RateLimit (RateLimit, FixedWindow, IPAddressPolicy)
import Data.Time.TypeLevel (TimePeriod (Second))
import qualified Servant.Auth.Server as SA
import Data.Int (Int64)

data AuthApi route = AuthApi
  { _authApiGenerateToken ::
      route
        :- "token"
          :> "generate"
          :> RateLimit (FixedWindow (Second 1) 1) (IPAddressPolicy "fixed")
          :> Capture "key" InstitutionKey
          :> Get '[JSON] (Response AuthToken),
    _authApiResetPasswordLink ::
      route
        :- "password"
          :> "reset"
          :> "link"
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> Put '[JSON] (Response (Maybe Int64)),
    _authApiResetPasswordNew ::
      route
        :- "password"
          :> "reset"
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> ReqBody '[JSON] NewPassword
          :> Post '[JSON] (Response Bool),
    _authApiSendAuthCode ::
      route
        :- "code"
          :> ReqBody '[JSON] Credentials
          :> Put '[JSON] (Response AuthCodeHash),
    _authApiResendAuthCode ::
      route
        :- "code"
          :> "resend"
          :> ReqBody '[JSON] ResendCode
          :> Put '[JSON] (Response AuthCodeHash),        
    _authApiLogin ::
      route
        :- "login"
          :> Capture "auth_type" AuthType
          :> ReqBody '[JSON] AuthCode
          :> Post '[JSON] (Response AuthToken),
    _authApiLogout ::
      route
        :- "logout"
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> Post '[JSON] (Response ())
  }
  deriving stock (Generic)
