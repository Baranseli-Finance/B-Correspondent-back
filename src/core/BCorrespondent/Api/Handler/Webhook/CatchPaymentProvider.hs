module BCorrespondent.Api.Handler.Webhook.CatchPaymentProvider (catch) where

import BCorrespondent.Transport.Model.Webhook (PaymentProvider)
import BCorrespondent.Transport.Payload (Payload)
import BCorrespondent.Transport.Response (Response)
import Katip.Handler (KatipHandlerM)

catch :: PaymentProvider -> Payload -> KatipHandlerM (Response ())
catch _ _ = undefined