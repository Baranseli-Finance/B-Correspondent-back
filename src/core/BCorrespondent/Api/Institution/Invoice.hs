{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Institution.Invoice (InvoiceApi (..)) where

import BCorrespondent.Transport.Model.Invoice (InvoiceRegisterRequest, InvoiceRegisterResponse)
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Auth (AuthenticatedUser, JWT, Role (..))
import Servant.API.Extended
import Servant.API.Generic (Generic)
import qualified Servant.Auth.Server as SA
import RateLimit (RateLimit, FixedWindow, KeyPolicy)
import Data.Time.TypeLevel (TimePeriod (Second))

newtype InvoiceApi route = 
        InvoiceApi
        { _invoiceApiRegister ::
            route
              :- "register"
                :> RateLimit (FixedWindow (Second 1) 20) (KeyPolicy "Token")
                :> SA.Auth '[JWT] (AuthenticatedUser Source)
                :> ReqBody '[JSON] [InvoiceRegisterRequest]
                :> Put '[JSON] (Response [InvoiceRegisterResponse])
        }
        deriving stock (Generic)