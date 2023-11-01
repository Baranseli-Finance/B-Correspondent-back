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
import Servant.API.Extended (JSON, Post, ReqBody, type (:>), Capture)
import Servant.API.Generic (Generic, GenericMode (type (:-)))
import RateLimit (RateLimit, FixedWindow, IPAddressPolicy)
import Data.Time.TypeLevel (TimePeriod (Second))

data WebhookApi route = 
     WebhookApi
     {_webhookApiCatchGithub ::
        route
            :- "github"
            :> RateLimit (FixedWindow (Second 1) 10) (IPAddressPolicy "fixed")
            :> ReqBody '[JSON] Payload
            :> Post '[JSON] (Response ()),
      _webhookApiCatchPaymentProvider ::
        route
            :- Capture "provider" PaymentProvider
            :> RateLimit (FixedWindow (Second 1) 10) (IPAddressPolicy "fixed")
            :> ReqBody '[JSON] Payload
            :> Post '[JSON] (Response ())
     }
     deriving stock (Generic)