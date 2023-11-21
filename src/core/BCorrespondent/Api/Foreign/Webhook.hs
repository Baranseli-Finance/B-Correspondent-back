{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Foreign.Webhook (WebhookApi (..)) where

import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Payload (Payload)
import BCorrespondent.Transport.Model.Webhook (PaymentProvider)
import BCorrespondent.Auth (AuthenticatedUser, JWT, Role (Sink))
import Servant.API.Extended (JSON, Post, ReqBody, type (:>), Capture)
import Servant.API.Generic (Generic, GenericMode (type (:-)))
import RateLimit (RateLimit, FixedWindow, IPAddressPolicy)
import Data.Time.TypeLevel (TimePeriod (Second))
import qualified Servant.Auth.Server as SA

newtype WebhookApi route = 
     WebhookApi
     {_webhookApiCatchPaymentProvider ::
        route
            :- RateLimit (FixedWindow (Second 1) 10) (IPAddressPolicy "fixed")
            :> SA.Auth '[JWT] (AuthenticatedUser Sink)
            :> Capture "provider" PaymentProvider
            :> ReqBody '[JSON] Payload
            :> Post '[JSON] (Response ())
     }
     deriving stock (Generic)