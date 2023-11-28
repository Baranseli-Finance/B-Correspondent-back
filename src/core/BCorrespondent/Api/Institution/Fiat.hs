{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Institution.Fiat (FiatApi (..)) where

import BCorrespondent.Transport.Model.Institution 
       (Withdraw, InitWithdrawal, WithdrawResult, WithdrawalHistory, WithdrawalCode)
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Auth (AuthenticatedUser, JWT, Role (..))
import Servant.API.Extended
import Servant.API.Generic (Generic)
import qualified Servant.Auth.Server as SA
import RateLimit (RateLimit, FixedWindow, KeyPolicy)
import Data.Time.TypeLevel (TimePeriod (Second))


data FiatApi route = 
     FiatApi
     { _fiatApiRegisterWithdrawal ::
         route
          :- "withdraw"
          :> "register"
          :> RateLimit (FixedWindow (Second 1) 1) (KeyPolicy "Token")
          :> SA.Auth '[JWT] (AuthenticatedUser 'Writer)
          :> ReqBody '[JSON] Withdraw
          :> Post '[JSON] (Response ()),
       _fiatApiConfirmWithdrawal ::
         route
          :- "withdraw"
          :> "confirm"
          :> RateLimit (FixedWindow (Second 1) 1) (KeyPolicy "Token")
          :> SA.Auth '[JWT] (AuthenticatedUser 'Writer)
          :> ReqBody '[JSON] WithdrawalCode
          :> Put '[JSON] (Response WithdrawResult),    
       _fiatApiInitWithdrawal ::
         route
          :- "withdraw"
          :> "init"
          :> RateLimit (FixedWindow (Second 1) 50) (KeyPolicy "Token")
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> Get '[JSON] (Response InitWithdrawal),
      _fiatApiGetWithdrawalHistoryPage ::
         route
          :- "withdraw"
          :> "history"
          :> "page"
          :> RateLimit (FixedWindow (Second 1) 50) (KeyPolicy "Token")
          :> SA.Auth '[JWT] (AuthenticatedUser 'Reader)
          :> QueryParam' '[Required, Strict] "page" Int
          :> Get '[JSON] (Response (Maybe WithdrawalHistory)),    
       _fiatApiTransactionOrder ::
         route
          :- "transaction"
          :> "order"
          :> RateLimit (FixedWindow (Second 1) 1) (KeyPolicy "Token")
          :> SA.Auth '[JWT] (AuthenticatedUser 'Writer)
          :> Post '[JSON] (Response ())
     }
     deriving stock (Generic)