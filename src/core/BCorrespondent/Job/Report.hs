{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE BangPatterns #-}

module BCorrespondent.Job.Report (makeDailyInvoices) where

import BCorrespondent.Statement.Report (fetchDailyInvoices, DailyInvoices (..))
import BCorrespondent.ServerM (ServerM)
import BCorrespondent.EnvKeys (Sendgrid (..), Person (..))
import Katip
import Control.Concurrent.Lifted (threadDelay)
import Database.Transaction (transactionM, statement)
import Katip.Handler (hasqlDbPool, ask, sendGrid)
import Control.Lens ((^.), (<&>))
import Control.Monad.Time (currentTime)
import Data.Time.Clock (utctDay)
import Control.Monad (when, void, forever)
import Control.Monad.Trans.State.Strict (evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (for_)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Control.Monad.IO.Class (liftIO)
import "sendgrid" OpenAPI.Common
import OpenAPI.Types.ToEmailArray (mkToEmailArrayItem, toEmailArrayItemName)
import OpenAPI.Operations.POSTMailSend
  ( mkPOSTMailSendRequestBody,
    mkPOSTMailSendRequestBodyContentsendgrid,
    mkPOSTMailSendRequestBodyPersonalizationssendgrid,
    pOSTMailSend,
    pOSTMailSendRequestBodyPersonalizationssendgridSendAt,
    pOSTMailSendRequestBodyPersonalizationssendgridSubject
  )
import OpenAPI.Types.FromEmailObject (mkFromEmailObject, fromEmailObjectName)
import Data.Coerce (coerce)
import Data.String.Conv (toS)


makeDailyInvoices :: Int -> Int -> KatipContextT ServerM ()
makeDailyInvoices freqBase freq = do
  tm <-currentTime
  let !initDay = utctDay tm
  flip evalStateT initDay $ do
    forever $ do  
      threadDelay $ freq * freqBase
      hasql <- fmap (^. hasqlDbPool) ask
      cfg <- fmap (^.sendGrid) ask 
      go hasql cfg
  where
    go hasql cfg = do
      currDay <- get
      tm <- currentTime
      let !day = utctDay tm
      when (day /= currDay) $ do 
        put day
        lift $ do
          dbResp <- transactionM hasql $ 
            statement fetchDailyInvoices currDay
          for_ dbResp $ \DailyInvoices {..} ->
            for_ cfg $ \(Sendgrid {..}, sendgrid) -> do
              tm <- fmap (fromIntegral . systemSeconds) $ liftIO $ getSystemTime
              let report =
                     "total: " <> toS (show dailyInvoicesTotal)
                     <> ", registered: " <> toS (show dailyInvoicesRegistered)
                     <> ", forwarded to Elekse: " <> toS (show dailyInvoicesForwarded)
                     <> ", processed by Elekse: " <> toS (show dailyInvoicesProcessed)
                     <> ", confirmed: " <> toS (show dailyInvoicesConfirmed)
                     <> ", declined: " <> toS (show dailyInvoicesDeclined)
                     <> ", discrepancy between total and status: "
                     <> toS (show (dailyInvoicesTotal))
                     <> " ~ " <> 
                     toS (show (dailyInvoicesRegistered + 
                                dailyInvoicesForwarded + 
                                dailyInvoicesProcessed + 
                                dailyInvoicesConfirmed + 
                                dailyInvoicesDeclined))
              let recipient = 
                    sendgridPersons <&> \Person {..} -> 
                      (mkToEmailArrayItem personEmail) 
                      {toEmailArrayItemName = Just personPersonalization }                  
              let reqBody =
                    mkPOSTMailSendRequestBody 
                    [mkPOSTMailSendRequestBodyContentsendgrid "text/plain" report]
                    ((mkFromEmailObject (coerce sendgridIdentity)) { fromEmailObjectName = Just "admin"})
                    [(mkPOSTMailSendRequestBodyPersonalizationssendgrid recipient)
                    { pOSTMailSendRequestBodyPersonalizationssendgridSendAt = Just tm,
                        pOSTMailSendRequestBodyPersonalizationssendgridSubject = 
                        Just $ "daily report for " <> toS (show currDay)
                    } ] $
                    "daily report for " <> toS (show currDay)
              liftIO $ void $ runWithConfiguration sendgrid (pOSTMailSend (Just reqBody))       
