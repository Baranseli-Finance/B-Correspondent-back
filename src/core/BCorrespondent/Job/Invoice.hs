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

module BCorrespondent.Job.Invoice (forwardToPaymentProvider) where

import qualified BCorrespondent.Institution.Webhook.Detail.Tochka as Tochka
import qualified BCorrespondent.Institution.Query.Invoice as Q
import qualified BCorrespondent.Institution.Query as Q
import BCorrespondent.Statement.Invoice 
       (getInvoicesToBeSent,
        setInvoiceInMotion,
        InvoiceToBeSent,
        QueryCredentials (..)
       )
import BCorrespondent.Statement.Delivery (addAttempt)
import qualified BCorrespondent.Statement.Delivery as D (TableRef (Invoice))
import qualified BCorrespondent.Statement.Webhook as Webhook
import BCorrespondent.ServerM
import BCorrespondent.Transport.Model.Invoice 
       (InvoiceToPaymentProvider (..),
        invoiceToPaymentProviderAmount,
        invoiceToPaymentProviderCurrency
       )
import BCorrespondent.Notification (Invoice (..), makeS)
import Katip
import Network.HTTP.Client (Manager)
import BuildInfo (location)
import Control.Monad (when, forever)
import Control.Concurrent.Lifted (threadDelay)
import Katip.Handler
import Control.Lens ((^.), (<&>))
import Database.Transaction (statement, transactionM)
import Data.Foldable (for_)
import Data.Either (partitionEithers)
import qualified Data.Text as T
import Data.String.Conv (toS)
import qualified Control.Concurrent.Async.Lifted as Async
import Data.Aeson (Value, toJSON)
import Data.Aeson.WithField (WithField (..))
import Data.Int (Int64)
import Data.Bifunctor (bimap, second)
import Data.Tuple.Extended (sel3, consT)
import GHC.Exts (groupWith, the)
import Data.UUID (UUID)
import Data.Time.Clock (UTCTime)
import Data.Traversable (for, forM)
import Data.Maybe (fromMaybe)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Request (forConcurrentlyNRetry)
import Data.Either (isRight)


forwardToPaymentProvider :: Int -> Int -> KatipContextT ServerM ()
forwardToPaymentProvider freqBase freq =
  forever $ do
    threadDelay $ freq * freqBase
    hasql <- fmap (^. hasqlDbPool) ask
    manager <- fmap (^.httpReqManager) ask
    go hasql manager
  where
    go hasql manager = do
      res <- transactionM hasql $ statement getInvoicesToBeSent 20
      case res of
        Right xs -> do
          yss <- Async.forConcurrently xs $ 
            \(ident, cred, zs) ->
              fmap (map (second (consT ident))) $
                sendInvoices manager Q.queries cred zs
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
              for_ (map sel3 os) $ statement setInvoiceInMotion . flip (:) []
              forM es $ \(ident, error) ->
                fmap (ident,) $ statement addAttempt (D.Invoice, ident, error)
            let notifParams = 
                  [ (the ident, body)
                    | (ident, body, _, _, _) <- os,
                      then group by ident using groupWith 
                  ]
            let webhookParams =
                  [ (the ident, val)
                    | (ident, _, _, external, tm) <- os,
                    let val = mkWebhookMsg ident external tm,
                    then group by ident using groupWith
                  ]   
            for_ notifParams $ uncurry (makeS @"invoice_forwarded")
            for_ webhookParams $ \(ident, xse) -> do 
              let (_, os) = partitionEithers xse
              transactionM hasql $ statement Webhook.insert (ident,os)
            for_ es' $ \(ident, is_stuck) -> 
              when is_stuck $ 
                $(logTM) ErrorS $ 
                  logStr @T.Text $ 
                    $location <> " invoice " <>
                    toS (show ident) <> 
                    " has been stuck forwarding to payment provider"
        Left err -> $(logTM) CriticalS $ logStr @T.Text $ $location <> ":forwardToPaymentProvider: decode error ---> " <> toS err


sendInvoices :: Manager -> [(Int64, Q.Query)] -> Maybe QueryCredentials -> [InvoiceToBeSent] -> KatipContextT ServerM [Either (Int64, T.Text) (Invoice, Int64, UUID, UTCTime)]
sendInvoices _ _ Nothing xs = for xs $ \(WithField _ (WithField ident _)) -> pure $ Left (ident, "payment provider credentials aren't set")
sendInvoices manager queries (Just QueryCredentials {..}) xs = do
  let mkErrorMsg msg = xs <&> \(WithField _ (WithField ident _)) -> Left (ident, msg) 
  fmap (fromMaybe (mkErrorMsg "provider not found")) $
    for (lookup provider queries) $ \(Q.Query {fetchToken, makeRequest}) -> do
      authRes <- fetchToken manager login password
      let withAuth (Left error) = pure $ mkErrorMsg $ toS error
          withAuth (Right token) = 
            forConcurrentlyNRetry 1 10 2 (pure . isRight) xs $ send makeRequest token
      withAuth authRes
  where
    send
      query
      token
      (WithField external
       (WithField ident
        (WithField
         textIdent
         invoice@InvoiceToPaymentProvider 
         {invoiceToPaymentProviderAmount = amount, 
          invoiceToPaymentProviderCurrency = currency}))) = do
      $(logTM) InfoS $ logStr @T.Text $ $location <> " invoice to payment provider:  " <> toS (show invoice)
      let notifBody = Invoice textIdent amount currency
      let mkResp = bimap ((ident,) . toS) ((notifBody, ident, external,) . Q.acceptedAt)
      fmap mkResp $ query manager Q.path token invoice
  
mkWebhookMsg :: Int64 -> UUID -> UTCTime -> Either String Value
mkWebhookMsg 1 external tm = 
   let formatTm = toS . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.00Z"
       message = Tochka.Invoice external (formatTm tm) "accepted"
   in Right $ toJSON $ (Tochka.defRequest message) { Tochka.requestMethod = Tochka.Registered }
mkWebhookMsg ident _ _ = Left $ "cannot make webhook value for institution " <> show ident