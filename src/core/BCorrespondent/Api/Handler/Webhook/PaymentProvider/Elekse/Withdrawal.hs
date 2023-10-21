module BCorrespondent.Api.Handler.Webhook.PaymentProvider.Elekse.Withdrawal (handle) where

import BCorrespondent.Transport.Model.Institution 
       (WithdrawalPaymentProviderResponse (..), WithdrawalPaymentProviderResponseStatus (..))
import BCorrespondent.Statement.Institution (modifyWalletAfterWebhook)
import BCorrespondent.Transport.Model.Institution 
       (WithdrawalStatus (Confirmed, Declined))
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, ask)
import BCorrespondent.Transport.Response (Response (Ok))
import Database.Transaction (transactionM, statement)
import Control.Lens ((^.))

handle :: WithdrawalPaymentProviderResponse -> KatipHandlerM (Response ())
handle WithdrawalPaymentProviderResponse 
       {withdrawalPaymentProviderResponseStatus = status,
       withdrawalPaymentProviderResponseExternalId = externalId} = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  let mkParams | status == WithdrawalPaymentProviderResponseStatusOk = (Confirmed, True, externalId)
               | otherwise = (Declined, False, externalId)
  fmap (const (Ok ())) $ transactionM hasql $ statement modifyWalletAfterWebhook mkParams