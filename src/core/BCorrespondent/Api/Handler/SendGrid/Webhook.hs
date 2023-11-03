{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module BCorrespondent.Api.Handler.SendGrid.Webhook (catch) where

import BCorrespondent.Statement.Mail (fetchMail, update)
import BCorrespondent.Transport.Model.SendGrid 
       (DeliveryEvent (..), DeliveryWStatus (Delivered))
import BCorrespondent.Transport.Payload (Payload (..))
import BCorrespondent.Transport.Response (Response (Ok))
import Database.Transaction (transactionM, statement)
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, smtpCfg, ask)
import Data.Aeson (eitherDecode, encode, Value (Object))
import BuildInfo (location)
import Katip
import Data.Either.Combinators (whenLeft)
import Data.Foldable (for_)
import Data.String (fromString)
import Control.Lens ((<&>), (^.))
import Control.Monad (when, void)
import Data.String.Conv (toS)
import Data.Maybe (fromMaybe)
import qualified Mail.SMTP as SMTP (send)
import Data.Tuple.Extended (uncurryT)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Exception.Lifted as E
import Control.Exception (SomeException)

catch :: [Payload]-> KatipHandlerM (Response ())
catch payloads = do
  $(logTM) DebugS $ fromString $ $location <> show payloads 
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
      when (deliveryEventEvent /= Delivered) $ do
        $(logTM) ErrorS $
          fromString $
            $location <> " email for " <> 
            toS deliveryEventEmail <> 
            " cannot be delivered due to " <>
            toS (fromMaybe mempty deliveryEventReason)
        hasql <- fmap (^. katipEnv . hasqlDbPool) ask
        smtp <- fmap (^. katipEnv . smtpCfg) ask
        for_ smtp $ \cfg ->
          for_ deliveryEventIdent $ \ident -> do
            mailSet <- transactionM hasql $ statement fetchMail ident
            for_ mailSet $ \set -> do 
              flip (E.handle @_ @SomeException) (liftIO (uncurryT (SMTP.send cfg) set)) $ \error -> do
                void $ transactionM hasql $ statement update (ident, Just (toS (show error)))
                $(logTM) ErrorS $
                  fromString $
                  $location <> " email for " <> 
                  toS deliveryEventEmail <>
                  " cannot be delivered due to " <>
                  toS (show error)
              void $ transactionM hasql $ statement update (ident, deliveryEventReason)