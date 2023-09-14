{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Foreign.SendGrid (SendGridApi (..)) where

import BCorrespondent.Api.Controller.SendGrid.SendMail (SendGridSendMailRequest)
import BCorrespondent.Transport.Response (Response)
import Servant.API.Extended (JSON, Post, ReqBody, type (:>))
import Servant.API.Generic (Generic, GenericMode (type (:-)))

newtype SendGridApi route = SendGridApi
  { _sendGridApiSendMail ::
      route
        :- "send"
          :> ReqBody '[JSON] SendGridSendMailRequest
          :> Post '[JSON] (Response ())
  }
  deriving stock (Generic)
