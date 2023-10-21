module BCorrespondent.Api.Handler.Webhook.PaymentProvider.Elekse.Payload where

import BCorrespondent.Transport.Model.Transaction (TransactionFromPaymentProvider)
import BCorrespondent.Transport.Model.Institution (WithdrawalPaymentProviderResponse)

data Payload = 
         TransactionPayload TransactionFromPaymentProvider 
       | WithdrawalPayload  WithdrawalPaymentProviderResponse
