{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Foreign
  ( ForeignApi (..),
    module SendGrid,
    module Github,
    module Webhook
  )
where

import BCorrespondent.Api.Foreign.SendGrid as SendGrid
import BCorrespondent.Api.Foreign.Github as Github
import BCorrespondent.Api.Foreign.Webhook as Webhook
import Servant.API.Extended (AsApi, ToServant, type (:>))
import Servant.API.Generic (Generic, GenericMode (type (:-)))

data ForeignApi route = ForeignApi
  { _foreignApiSendGrid ::
      route
        :- "sendgrid"
          :> ToServant SendGridApi AsApi,
    _foreignApiGithub ::
      route
        :- "github"
          :> ToServant GithubApi AsApi,
    _foreignApiWebhook ::
      route
        :- "webhook"
          :> ToServant WebhookApi AsApi
  }
  deriving stock (Generic)
