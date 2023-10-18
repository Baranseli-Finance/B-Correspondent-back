{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Institution.Fiat (FiatApi (..)) where

import BCorrespondent.Transport.Model.Institution (Withdraw, Balances)
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Auth (AuthenticatedUser, JWT, Role (..))
import Servant.API.Extended
import Servant.API.Generic (Generic)
import qualified Servant.Auth.Server as SA
import RateLimit (RateLimit, FixedWindow, KeyPolicy)
import Data.Time.TypeLevel (TimePeriod (Second))

data FiatApi route = 
     FiatApi
     { _fiatApiWithdraw ::
         route
          :- "withdraw"
          :> RateLimit (FixedWindow (Second 1) 1) (KeyPolicy "Token")
          :> SA.Auth '[JWT] (AuthenticatedUser 'Writer)
          :> ReqBody '[JSON] Withdraw
          :> Post '[JSON] (Response ()),
       _fiatApiGetBalances ::
         route
          :- "balances"
          :> RateLimit (FixedWindow (Second 1) 1) (KeyPolicy "Token")
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> Get '[JSON] (Response Balances),
       _fiatApiTransactionOrder ::
         route
          :- "transaction"
          :> "order"
          :> RateLimit (FixedWindow (Second 1) 1) (KeyPolicy "Token")
          :> SA.Auth '[JWT] (AuthenticatedUser 'Writer)
          :> Post '[JSON] (Response ())
     }
     deriving stock (Generic)