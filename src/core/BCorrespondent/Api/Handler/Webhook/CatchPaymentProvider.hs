{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BCorrespondent.Api.Handler.Webhook.CatchPaymentProvider (catch) where

import BCorrespondent.Transport.Model.Transaction (OkTransaction)
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
import BuildInfo (location)
import Katip
import Data.String (fromString)
import Data.Text (Text, intercalate)
import Data.String.Conv (toS)
import Validation (Validation, eitherToValidation, partitionValidations)


catch :: PaymentProvider -> Payload -> KatipHandlerM (Response ())
catch Elekse payload = do
  let (es, as) = partitionValidations alt
  case as of 
    [] ->
      let msg = intercalate ", " $ map toS es
          errorResp = Error (Just 400) $ asError @Text ("decode error: " <> msg)
      in fmap (const errorResp) $
        $(logTM) ErrorS $
          fromString $ 
          $location <> ": error ---> " <> 
          toS msg <> ", payload: " <> show payload
    [x] ->
      case x of
        Elekse.TransactionPayload body -> 
          Elekse.Transaction.handle body
        Elekse.WithdrawalPayload body -> 
          Elekse.Withdrawal.handle body
    _ -> pure $ Error (Just 500) $ asError @Text ("decode error: payload has been resolved into more then one type")
  where
    alt =
         [ fmap Elekse.TransactionPayload 
           (reify @OkTransaction payload)
         , fmap Elekse.WithdrawalPayload 
           (reify @WithdrawalPaymentProviderResponse payload)
         ]

reify :: forall a . FromJSON a => Payload -> Validation String a
reify = eitherToValidation . eitherDecode @a . encode