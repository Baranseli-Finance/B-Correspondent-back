{-# LANGUAGE TypeApplications #-}

module BCorrespondent.Api.Handler.Webhook.CatchPaymentProvider (catch) where

import BCorrespondent.Transport.Model.Transaction (TransactionFromPaymentProvider)
import qualified BCorrespondent.Api.Handler.Webhook.PaymentProvider.Elekse as Elekse
import BCorrespondent.Transport.Model.Webhook (PaymentProvider (..))
import BCorrespondent.Api.Handler.Utils (withLogEither) 
import BCorrespondent.Transport.Payload (Payload)
import BCorrespondent.Transport.Response (Response (Ok))
import Katip.Handler (KatipHandlerM)
import Data.Aeson (eitherDecode, encode)

catch :: PaymentProvider -> Payload -> KatipHandlerM (Response ())
catch Elekse payload = do 
  resp <- traverse Elekse.handle $ 
    eitherDecode
      @TransactionFromPaymentProvider $
    encode payload
  withLogEither resp $ const (pure (Ok ()))