{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Transaction (TransactionApi (..)) where

import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Auth (AuthenticatedUser, JWT)
import Servant.API.Extended
import Servant.API.Generic (Generic)
import qualified Servant.Auth.Server as SA

data TransactionApi route = TransactionApi
  { _transactionApiHistory ::
      route
        :- "history"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> Get '[JSON] (Response ()),
    _transactionApiDayTimeline ::
      route
        :- "timeline"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> Get '[JSON] (Response ())
  }
  deriving stock (Generic)
