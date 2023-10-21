module BCorrespondent.Api.Handler.Webhook.PaymentProvider.Elekse.Withdrawal (handle) where

import BCorrespondent.Transport.Model.Institution (WithdrawalPaymentProviderResponse)
import Katip.Handler (KatipHandlerM)
import BCorrespondent.Transport.Response (Response)

handle :: WithdrawalPaymentProviderResponse -> KatipHandlerM (Response ())
handle _ = undefined