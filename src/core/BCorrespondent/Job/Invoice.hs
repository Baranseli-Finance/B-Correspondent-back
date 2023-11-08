{-#LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module BCorrespondent.Job.Invoice (forwardToPaymentProvider, validateAgainstTransaction) where

import BCorrespondent.Statement.Invoice 
       (getInvoicesToBeSent, 
        insertFailedInvoices, 
        updateStatus,
        setInvoiceInMotion,
        getValidation,
        Status (Confirmed, Declined),
        Validation (..),
        InvoiceToBeSent
       )
import BCorrespondent.Statement.Institution (updateWallet)  
import BCorrespondent.Job.Utils (withElapsedTime)
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
import Database.Transaction (statement, transactionM)
import Data.Foldable (for_)
import Data.Either (partitionEithers)
import qualified Data.Text as T
import Data.String.Conv (toS)
import qualified Control.Concurrent.Async.Lifted as Async
import Data.Aeson.WithField (WithField (..))
import qualified Request as Request
import Data.Int (Int64)
import Data.Bifunctor (bimap, second)
import Data.Tuple.Extended (sel3, consT)
import GHC.Exts (groupWith, the)


forwardToPaymentProvider :: Int -> KatipContextT ServerM ()
forwardToPaymentProvider freq =
  forever $ do 
    threadDelay $ freq * 1_000_000
    withElapsedTime ($location <> ":forwardToPaymentProvider") $ do
      hasql <- fmap (^. hasqlDbPool) ask
      res <- transactionM hasql $ statement getInvoicesToBeSent ()
      case res of
        Right xs -> do
          manager <- fmap (^.httpReqManager) ask
          yss <- Async.forConcurrently xs $ 
            \(ident, zs) -> 
              fmap (map (second (consT ident))) $ 
                Async.forConcurrently zs $ 
                  sendInvoice manager

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
                    | (ident, body, _) <- os,
                      then group by ident using groupWith 
                  ]    
            for_ notifParams $ uncurry (makeS @"invoice_forwarded")
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


sendInvoice :: Manager -> InvoiceToBeSent -> KatipContextT ServerM (Either (Int64, T.Text) (Invoice, Int64))
sendInvoice 
  manager 
  (WithField ident 
   (WithField 
      textIdent 
      invoice@InvoiceToPaymentProvider 
      {invoiceToPaymentProviderAmount = amount, 
       invoiceToPaymentProviderCurrency = currency})) = do
    let notifBody = Invoice textIdent amount currency
    let mkResp = bimap ((ident,) . toS . show) (const (notifBody, ident))
    let onFailure = pure . Left . show
    $(logTM) InfoS $ logStr @T.Text $ $location <> " invoice to payment provider:  " <> toS (show invoice) 
    fmap mkResp $ Request.makePostReq @InvoiceToPaymentProvider mempty manager [] invoice onFailure

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