{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TypeApplications #-}

module BCorrespondent.Job.Invoice (forwardToPaymentProvider) where

import BCorrespondent.Statement.Invoice 
       (getInvoicesToBeSent, insertFailedInvoices, updateStatus, Status (ForwardedToPaymentProvider))
import BCorrespondent.Job.Utils (withElapsedTime)
import BCorrespondent.ServerM
import BCorrespondent.Transport.Model.Invoice (InvoiceToPaymentProvider)
import Katip
import BuildInfo (location)
import Control.Monad (forever)
import Control.Concurrent.Lifted (threadDelay)
import Katip.Handler
import Control.Lens ((^.), (<&>))
import Database.Transaction (statement, transactionM)
import Data.Foldable (for_)
import Data.Either (partitionEithers)
import qualified Data.Text as T
import Data.String.Conv (toS)
import qualified Control.Concurrent.Async.Lifted as Async
import Data.Aeson.WithField (WithField (..))
import qualified Request as Request 

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
          ys <- Async.forConcurrently xs $
            \(WithField _ invoice) -> do
              let req = Left $ Just $ invoice
              _ <- Request.make @InvoiceToPaymentProvider undefined manager [] Request.methodPost req
              undefined
          let (es, os) = partitionEithers ys
          for_ es $ \ident ->
            $(logTM) ErrorS $
            logStr @T.Text $
              $location <>
              ":forwardToElekse: --> \
              \ invoice failed to be sent, " <> 
              toS (show ident)
          transactionM hasql $ do 
            statement insertFailedInvoices es
            statement updateStatus $ os <&> \x -> (x, ForwardedToPaymentProvider)
        Left err -> $(logTM) CriticalS $ logStr @T.Text $ $location <> ":forwardToPaymentProvider: decode error ---> " <> toS err
