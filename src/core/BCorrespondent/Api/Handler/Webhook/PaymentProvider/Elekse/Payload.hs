module BCorrespondent.Api.Handler.Webhook.PaymentProvider.Elekse.Payload where

import BCorrespondent.Transport.Model.Transaction (OkTransaction)
import BCorrespondent.Transport.Model.Institution (WithdrawalPaymentProviderResponse)

data Payload = 
         TransactionPayload OkTransaction 
       | WithdrawalPayload  WithdrawalPaymentProviderResponse
