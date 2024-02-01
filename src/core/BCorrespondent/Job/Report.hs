{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE BangPatterns #-}

module BCorrespondent.Job.Report (makeDailyInvoices, getDay) where

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
import Control.Monad (void)
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
import Data.Time.Calendar.OrdinalDate (Day)


getDay :: KatipContextT ServerM Day 
getDay = fmap utctDay currentTime

makeDailyInvoices :: Int -> Int -> Day -> KatipContextT ServerM ()
makeDailyInvoices freqBase freq oldDay = do
  threadDelay $ freq * freqBase
  hasql <- fmap (^. hasqlDbPool) ask
  cfg <- fmap (^.sendGrid) ask
  newDay <- getDay
  if (newDay /= oldDay)
  then do  
    go hasql cfg newDay
    makeDailyInvoices freqBase freq newDay
  else makeDailyInvoices freqBase freq oldDay
  where
    go hasql cfg currDay = do
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
