module BCorrespondent.Api.Handler.Webhook.CatchElekse (catch) where

import BCorrespondent.Transport.Payload (Payload)
import BCorrespondent.Transport.Response (Response)
import Katip.Handler (KatipHandlerM)

catch :: Payload -> KatipHandlerM (Response ())
catch _ = undefined