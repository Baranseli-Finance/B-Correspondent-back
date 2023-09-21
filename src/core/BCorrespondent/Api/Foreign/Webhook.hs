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
import Servant.API.Extended (JSON, Post, ReqBody, type (:>))
import Servant.API.Generic (Generic, GenericMode (type (:-)))
import RateLimit (RateLimit, FixedWindow, IPAddressPolicy)
import Data.Time.TypeLevel (TimePeriod (Second))

newtype WebhookApi route = 
    WebhookApi
    { _webhookApidApiCatchElekse ::
        route
            :- "elekse"
            :> RateLimit (FixedWindow (Second 1) 10) (IPAddressPolicy "fixed")
            :> ReqBody '[JSON] Payload
            :> Post '[JSON] (Response ())
    }
    deriving stock (Generic)
