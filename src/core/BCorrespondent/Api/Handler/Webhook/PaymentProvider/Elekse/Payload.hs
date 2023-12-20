module BCorrespondent.Api.Handler.Webhook.PaymentProvider.Elekse.Payload where

import BCorrespondent.Transport.Model.Transaction (OkTransaction, FailedTransaction)
import BCorrespondent.Transport.Model.Institution (WithdrawalPaymentProviderResponse)

data Payload = 
         OkTransactionPayload OkTransaction
       | FailedTransactionPayload FailedTransaction
       | WithdrawalPayload  WithdrawalPaymentProviderResponse
