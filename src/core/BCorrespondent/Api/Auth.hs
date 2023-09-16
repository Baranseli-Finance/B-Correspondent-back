{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Auth (AuthApi (..)) where

import BCorrespondent.Transport.Model.Auth (AuthToken, InstitutionKey)
import BCorrespondent.Transport.Response (Response)
import Servant.API.Extended
import Servant.API.Generic (Generic)
import RateLimit (RateLimit, FixedWindow, IPAddressPolicy)
import Data.Time.TypeLevel (TimePeriod (Second))

newtype AuthApi route = AuthApi
  { _authApiGenerateToken ::
      route
        :- "token"
          :> "generate"
          :> RateLimit (FixedWindow (Second 1) 1) (IPAddressPolicy "fixed")
          :> Capture "key" InstitutionKey
          :> Post '[JSON] (Response AuthToken)
  }
  deriving stock (Generic)
