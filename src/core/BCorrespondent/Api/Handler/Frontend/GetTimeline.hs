{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Frontend.GetTimeline (handle) where

import BCorrespondent.Transport.Response (Response)
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM)

handle :: Auth.AuthenticatedUser 'Auth.Reader -> KatipHandlerM (Response ())
handle _ = undefined