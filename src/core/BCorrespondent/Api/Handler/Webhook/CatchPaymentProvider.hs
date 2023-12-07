{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BCorrespondent.Api.Handler.Webhook.CatchPaymentProvider (catch) where

import BCorrespondent.Transport.Model.Transaction (TransactionFromPaymentProvider)
import BCorrespondent.Transport.Model.Institution (WithdrawalPaymentProviderResponse)
import qualified BCorrespondent.Api.Handler.Webhook.PaymentProvider.Elekse.Payload as Elekse
import qualified BCorrespondent.Api.Handler.Webhook.PaymentProvider.Elekse.Transaction as Elekse.Transaction
import qualified BCorrespondent.Api.Handler.Webhook.PaymentProvider.Elekse.Withdrawal as Elekse.Withdrawal
import BCorrespondent.Transport.Model.Webhook (PaymentProvider (..))
import BCorrespondent.Transport.Payload (Payload)
import BCorrespondent.Transport.Error (asError)
import BCorrespondent.Transport.Response (Response (Error))
import Katip.Handler (KatipHandlerM)
import Data.Aeson (eitherDecode, encode, FromJSON)
import Data.Traversable (for)
import BuildInfo (location)
import Katip
import Data.String (fromString)
import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.String.Conv (toS)


catch :: PaymentProvider -> Payload -> KatipHandlerM (Response ())
catch Elekse payload = do
  handleResp <- for alt $ \case
    Elekse.TransactionPayload body -> 
      Elekse.Transaction.handle body
    Elekse.WithdrawalPayload body -> 
      Elekse.Withdrawal.handle body
  case handleResp of
    Right resp -> pure resp
    Left error ->
      let errorResp = 
            Error (Just 400) $ 
              asError @Text ("decode error: " <> toS error)
      in fmap (const errorResp) $
        $(logTM) ErrorS $
          fromString $ 
          $location <> ": error ---> " <> 
          error <> ", payload: " <> show payload
  where
    alt =
         fmap Elekse.TransactionPayload 
         (reify @TransactionFromPaymentProvider payload)
        <|>
          fmap Elekse.WithdrawalPayload 
          (reify @WithdrawalPaymentProviderResponse payload)

reify :: forall a . FromJSON a => Payload -> Either String a
reify = eitherDecode @a . encode