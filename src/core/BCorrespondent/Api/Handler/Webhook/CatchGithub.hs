{-# LANGUAGE TypeApplications #-}

module BCorrespondent.Api.Handler.Webhook.CatchGithub (catch) where

import BCorrespondent.Transport.Model.Github (Sha)
import BCorrespondent.Transport.Payload (Payload (..))
import BCorrespondent.Transport.Response (Response)
import Katip.Handler (KatipHandlerM)
import Data.Aeson (eitherDecode, encode, Value (Object))

catch :: Payload -> KatipHandlerM (Response ())
catch payload = error $ show $ eitherDecode @Sha $ encode $ Object $ getPayload payload