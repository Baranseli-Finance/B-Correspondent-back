{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module BCorrespondent.Api.Handler.SendGrid.Webhook (catch) where

import BCorrespondent.Transport.Model.SendGrid 
       (DeliveryEvent (..), DeliveryWStatus (Delivered))
import BCorrespondent.Transport.Payload (Payload (..))
import BCorrespondent.Transport.Response (Response (Ok))
import Katip.Handler (KatipHandlerM)
import Data.Aeson (eitherDecode, encode, Value (Object))
import BuildInfo (location)
import Katip
import Data.Either.Combinators (whenLeft)
import Data.Foldable (for_)
import Data.String (fromString)
import Control.Lens ((<&>))
import Control.Monad (when)
import Data.String.Conv (toS)
import Data.Maybe (fromMaybe)

catch :: [Payload]-> KatipHandlerM (Response ())
catch payloads = do 
  let val = 
         sequence $ 
           payloads <&> (
            eitherDecode @DeliveryEvent . 
            encode . 
            Object . 
            getPayload)
  whenLeft val $ \e -> 
    $(logTM) ErrorS $ 
      fromString $ 
        $location <> ", error: " <> e
  fmap Ok $ for_ val $ \xs -> 
    for_ xs $ \DeliveryEvent {..} ->
      when (deliveryEventEvent /= Delivered) $
        $(logTM) ErrorS $
          fromString $
            $location <> " email for " <> 
            toS deliveryEventEmail <> 
            " cannot be delivered due to " <>
            toS (fromMaybe mempty deliveryEventReason)