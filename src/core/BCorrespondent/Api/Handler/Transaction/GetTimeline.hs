module BCorrespondent.Api.Handler.Transaction.GetTimeline (handle) where

import BCorrespondent.Transport.Response (Response)
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM)

handle :: Auth.AuthenticatedUser  -> KatipHandlerM (Response ())
handle _ = undefined