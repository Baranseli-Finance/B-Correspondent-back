{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module BCorrespondent.Api.Handler.Webhook.CatchPaymentProvider (catch) where

import BCorrespondent.Transport.Model.Transaction (TransactionFromPaymentProvider)
import BCorrespondent.Transport.Model.Institution (WithdrawalPaymentProviderResponse)
import qualified BCorrespondent.Api.Handler.Webhook.PaymentProvider.Elekse.Payload as Elekse
import qualified BCorrespondent.Api.Handler.Webhook.PaymentProvider.Elekse.Transaction as Elekse.Transaction
import qualified BCorrespondent.Api.Handler.Webhook.PaymentProvider.Elekse.Withdrawal as Elekse.Withdrawal
import BCorrespondent.Transport.Model.Webhook (PaymentProvider (..))
import BCorrespondent.Transport.Payload (Payload)
import BCorrespondent.Transport.Response (Response (Ok))
import Katip.Handler (KatipHandlerM)
import Data.Aeson (eitherDecode, encode, FromJSON)
import Data.Foldable (for_)
import Data.Traversable (for)
import BuildInfo (location)
import Katip
import Data.String (fromString)
import Data.Either.Combinators (swapEither)
import Control.Monad (msum)


catch :: PaymentProvider -> Payload -> KatipHandlerM (Response ())
catch Elekse payload = do
  resp <- fmap swapEither $ for alt $ \case
    Elekse.TransactionPayload body -> 
      Elekse.Transaction.handle body
    Elekse.WithdrawalPayload body -> 
      Elekse.Withdrawal.handle body
  fmap (const (Ok ())) $ for_ resp $ \e -> 
    $(logTM) ErrorS $ 
      fromString $ 
        $location <> "error ---> " <> 
        e <> ", payload: " <> show payload
  where
    alt =
         msum 
         [ fmap Elekse.TransactionPayload $ 
             reify @TransactionFromPaymentProvider payload,
           fmap Elekse.WithdrawalPayload $  
             reify @WithdrawalPaymentProviderResponse payload
         ]

reify :: forall a . FromJSON a => Payload -> Either String a
reify = eitherDecode @a . encode