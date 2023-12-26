{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE BangPatterns #-}

module BCorrespondent.Job.Report (makeDailyInvoices) where

import BCorrespondent.Job.Utils (withElapsedTime, forever)
import BCorrespondent.Statement.Report (fetchDailyInvoices, DailyInvoices (..))
import BCorrespondent.ServerM (ServerM)
import BCorrespondent.EnvKeys (Sendgrid (..), Person (..))
import Katip
import BuildInfo (location)
import Control.Concurrent.Lifted (threadDelay)
import Database.Transaction (transactionM, statement)
import Katip.Handler (hasqlDbPool, ask, sendGrid)
import Control.Lens ((^.), (<&>))
import Control.Monad.Time (currentTime)
import Data.Time.Clock (utctDay)
import Control.Monad (when, void)
import Control.Monad.Trans.State.Strict (evalStateT, get, modify')
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


makeDailyInvoices :: Int -> KatipContextT ServerM ()
makeDailyInvoices freq = do 
  tm <-currentTime
  let !day = utctDay tm
  flip evalStateT day $ do
    forever $ do  
      threadDelay $ freq * 1_000_000
      currDay <- get
      tm <-currentTime
      let !day = utctDay tm
      when (day /= currDay) $ do 
        modify' (const day)
        lift $ withElapsedTime ($location <> ":makeDailyInvoices") $ do
          hasql <- fmap (^. hasqlDbPool) ask
          dbResp <- transactionM hasql $ 
            statement fetchDailyInvoices currDay
          for_ dbResp $ \DailyInvoices {..} -> do
            cfg <- fmap (^.sendGrid) ask
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
