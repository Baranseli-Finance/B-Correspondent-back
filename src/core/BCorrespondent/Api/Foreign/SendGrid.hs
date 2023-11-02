{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Foreign.SendGrid (SendGridApi (..)) where

import BCorrespondent.Api.Handler.SendGrid.Mail (SendGridSendMailRequest)
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Payload (Payload)
import Servant.API.Extended (JSON, Post, ReqBody, type (:>))
import Servant.API.Generic (Generic, GenericMode (type (:-)))

data SendGridApi route = SendGridApi
  { _sendGridApiSendMail ::
      route
        :- "send"
          :> ReqBody '[JSON] SendGridSendMailRequest
          :> Post '[JSON] (Response ()),
    _sendGridApiCatchWebhook ::
      route
        :- "webhook"
          :> ReqBody '[JSON] [Payload]
          :> Post '[JSON] (Response ())
  }
  deriving stock (Generic)
