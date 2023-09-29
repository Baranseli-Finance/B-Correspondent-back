{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Institution.Fiat (FiatApi (..)) where

import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Auth (AuthenticatedUser, JWT, Role (..))
import Servant.API.Extended
import Servant.API.Generic (Generic)
import qualified Servant.Auth.Server as SA

data FiatApi route = 
     FiatApi
     { _fiatApiWithdraw ::
        route
          :- "withdraw"
          :> SA.Auth '[JWT] (AuthenticatedUser 'Writer)
          :> Post '[JSON] (Response ()),
       _fiatApiTransactionOrder ::
        route
          :- "transaction"
          :> "order"
          :> SA.Auth '[JWT] (AuthenticatedUser 'Writer)
          :> Post '[JSON] (Response ())
     }
     deriving stock (Generic)