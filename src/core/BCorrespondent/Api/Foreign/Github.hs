{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Foreign.Github (GithubApi (..)) where

import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Payload (Payload)
import Servant.API.Extended (JSON, Post, ReqBody, type (:>))
import Servant.API.Generic (Generic, GenericMode (type (:-)))

newtype GithubApi route = GithubApi
  { _githubApiCatchWebhook ::
      route
        :- ReqBody '[JSON] Payload
        :> Post '[JSON] (Response ())
  }
  deriving stock (Generic)
