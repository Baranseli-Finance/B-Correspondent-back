{-#LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}

module BCorrespondent.Job.Invoice (forwardToPaymentProvider, validateAgainstTransaction) where

import qualified BCorrespondent.Job.Invoice.Provider.Elekse as Elekse (make)
import BCorrespondent.Job.Invoice.Query (Query (..))
import qualified BCorrespondent.Job.Invoice.Query as Q
import BCorrespondent.Statement.Invoice 
       (getInvoicesToBeSent, 
        insertFailedInvoices, 
        updateStatus,
        setInvoiceInMotion,
        getValidation,
        Status (Confirmed, Declined),
        Validation (..),
        InvoiceToBeSent,
        QueryCredentials (..)
       )
import BCorrespondent.Statement.Institution (updateWallet)  
import BCorrespondent.Job.Utils (withElapsedTime)
import qualified BCorrespondent.Statement.Webhook as Webhook
import BCorrespondent.ServerM
import BCorrespondent.Transport.Model.Invoice 
       (InvoiceToPaymentProvider (..), 
        Fee (..),
        invoiceToPaymentProviderAmount,
        invoiceToPaymentProviderCurrency
       )
import BCorrespondent.Notification (Invoice (..), makeS)
import Katip
import Network.HTTP.Client (Manager)
import BuildInfo (location)
import Control.Monad (forever, when)
import Control.Concurrent.Lifted (threadDelay)
import Katip.Handler
import Control.Lens ((^.))
import Database.Transaction (statement, transactionM, ParamsShow (..))
import Data.Foldable (for_)
import Data.Either (partitionEithers)
import qualified Data.Text as T
import Data.String.Conv (toS)
import qualified Control.Concurrent.Async.Lifted as Async
import Data.Aeson.WithField (WithField (..))
import Data.Int (Int64)
import Data.Bifunctor (bimap, second)
import Data.Tuple.Extended (sel3, consT)
import GHC.Exts (groupWith, the)
import Data.UUID (UUID)
import Data.Time.Clock (UTCTime)
import Data.Aeson (ToJSON)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)
import Data.Traversable (for)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Time.Format (formatTime, defaultTimeLocale)


data WebhookMsg =
     WebhookMsg 
     { externalId :: UUID,
       createdAt :: T.Text,
       status :: T.Text
     }
     deriving stock (Generic, Show)
     deriving (ToJSON)
      via WithOptions '[FieldLabelModifier '[CamelTo2 "_"]] WebhookMsg

instance ParamsShow WebhookMsg where
  render (WebhookMsg ident tm status) = render ident <> render tm <> render status

forwardToPaymentProvider :: Int -> KatipContextT ServerM ()
forwardToPaymentProvider freq = do
  let queries = [(2, Elekse.make)]
  forever $ do 
    threadDelay $ freq * 1_000_000
    withElapsedTime ($location <> ":forwardToPaymentProvider") $ do
      hasql <- fmap (^. hasqlDbPool) ask
      res <- transactionM hasql $ statement getInvoicesToBeSent ()
      case res of
        Right xs -> do
          manager <- fmap (^.httpReqManager) ask
          yss <- Async.forConcurrently xs $ 
            \(ident, cred, zs) -> 
              fmap (map (second (consT ident))) $ 
                Async.forConcurrently zs $ 
                  sendInvoice manager queries cred

          for_ yss $ \ys -> do
            let (es, os) = partitionEithers ys
            for_ es $ \(ident, error) ->
              $(logTM) ErrorS $
              logStr @T.Text $
                $location <>
                ":forwardToElekse: --> \
                \ invoice failed to be sent, " <> 
                toS (show ident) <> ", error: " <> error
            es' <- transactionM hasql $ do
              statement setInvoiceInMotion $ map sel3 os
              statement insertFailedInvoices es
            let notifParams = 
                  [ (the ident, body)
                    | (ident, body, _, _, _) <- os,
                      then group by ident using groupWith 
                  ]
            let webhookParams =
                  [ (the ident, message)
                    | (ident, _, _, external, tm) <- os,
                      let formatTm = toS . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.00Z",
                      let message = WebhookMsg external (formatTm tm) "accepted",
                      then group by ident using groupWith 
                  ]   
            for_ notifParams $ uncurry (makeS @"invoice_forwarded")
            for_ webhookParams $ transactionM hasql . statement Webhook.insert
            for_ es' $ \(ident, is_stuck) -> 
              when is_stuck $ 
                $(logTM) ErrorS $ 
                  logStr @T.Text $ 
                    $location <> " invoice " <>
                    toS (show ident) <> 
                    " has been stuck forwarding to payment provider"
        Left err -> 
          $(logTM) CriticalS $ 
            logStr @T.Text $ 
              $location <> ":forwardToPaymentProvider: decode error ---> " <> toS err


sendInvoice :: Manager -> [(Int64, Query)] -> Maybe QueryCredentials -> InvoiceToBeSent -> KatipContextT ServerM (Either (Int64, T.Text) (Invoice, Int64, UUID, UTCTime))
sendInvoice _ _ Nothing (WithField _ (WithField ident _)) = 
  pure $ Left (ident, "payment provider credentials aren't set")
sendInvoice
  manager
  queries
  (Just QueryCredentials {..})
  (WithField external
   (WithField ident
    (WithField
      textIdent
      invoice@InvoiceToPaymentProvider 
      {invoiceToPaymentProviderAmount = amount, 
       invoiceToPaymentProviderCurrency = currency}))) = do
    $(logTM) InfoS $ logStr @T.Text $ $location <> " invoice to payment provider:  " <> toS (show invoice)    
    let msg = "provider not found"
    let notifBody = Invoice textIdent amount currency
    let mkResp = bimap ((ident,) . toS) ((notifBody, ident, external,) . Q.acceptedAt)
    fmap (fromMaybe (Left (ident, msg))) $ 
      for (lookup provider queries) $ \(Query {query}) -> 
        fmap mkResp $ liftIO $ query manager login password invoice

validateAgainstTransaction :: Int -> KatipContextT ServerM ()
validateAgainstTransaction freq = 
  forever $ do 
    threadDelay $ freq * 1_000_000
    withElapsedTime ($location <> ":validateAgainstTransaction") $ do
      hasql <- fmap (^. hasqlDbPool) ask
      res <- transactionM hasql $ statement getValidation ()
      case res of
        Right xs -> do
          hasql <- fmap (^. hasqlDbPool) ask
          recordsUpdated <- 
            transactionM hasql $ do
              let ys = map validate xs
              statement updateStatus ys
              let zs = flip filter ys $ \(_, s) -> s == Confirmed
              statement updateWallet $ fst $ unzip zs
          $(logTM) InfoS $ logStr @T.Text $ $location <> " there are " <> T.pack (show recordsUpdated) <> " wallets updated"   
        Left err -> $(logTM) CriticalS $ logStr @T.Text $ $location <> ":validateAgainstTransaction: decode error ---> " <> toS err

validate :: Validation -> (Int64, Status)
validate Validation {..} 
  | validationInvoiceAmount == validationTransactionAmount
    && validationInvoiceCurrency == validationTransactionCurrency
    && validationInvoiceFee == OUR
    = (validationInvoiceIdent, Confirmed)
  | validationInvoiceAmount /= validationTransactionAmount
    && validationInvoiceCurrency == validationTransactionCurrency
    && validationInvoiceFee == SHA 
    = (validationInvoiceIdent, Confirmed)
  | otherwise = (validationInvoiceIdent, Declined)