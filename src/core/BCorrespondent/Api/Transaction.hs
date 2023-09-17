{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Transaction (TransactionApi (..)) where

import BCorrespondent.Transport.Model.Transaction
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Auth (AuthenticatedUser, JWT)
import Servant.API.Extended
import Servant.API.Generic (Generic)
import RateLimit (RateLimit, FixedWindow, KeyPolicy)
import Data.Time.TypeLevel (TimePeriod (Second))
import qualified Servant.Auth.Server as SA

data TransactionApi route = TransactionApi
  { _transactionApiNew ::
      route
        :- SA.Auth '[JWT] AuthenticatedUser
          :> ReqBody '[JSON] [TransactionNewRequest]
          :> Put '[JSON] (Response [TransactionId]),
    _transactionApiGetConfirmed ::
      route
        :- "list"
          :> RateLimit (FixedWindow (Second 1) 1) (KeyPolicy "Token")
          :> SA.Auth '[JWT] AuthenticatedUser
          :> ReqBody '[JSON] [TransactionId]
          :> Post '[JSON] (Response [TransactionConfirmed]),
    _transactionApiHistory ::
      route
        :- "history"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> Get '[JSON] (Response [Transaction])
  }
  deriving stock (Generic)
