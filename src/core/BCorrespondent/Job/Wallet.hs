{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE FlexibleContexts #-}

module BCorrespondent.Job.Wallet (withdraw, archive) where

import BCorrespondent.Transport.Model.Institution 
       (WithdrawalPaymentProviderRequest (..), WithdrawalStatus (Processing))
import BCorrespondent.Statement.Institution 
       (fetchWithdrawals, updateWithdrawalStatus, archiveWallets)
import BCorrespondent.ServerM (ServerM)
import Katip
import BuildInfo (location)
import Control.Concurrent.Lifted (threadDelay)
import Control.Lens ((^.), _1)
import Database.Transaction (statement, transactionM)
import Katip.Handler
import qualified Control.Concurrent.Async.Lifted as Async
import qualified Request as Request
import Data.Tuple.Extended (del1, uncurryT)
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.String.Conv (toS)
import qualified Data.Text as T
import Data.Int (Int64)
import Control.Monad (void, when, forever)
import Control.Monad.Time (currentTime)
import Data.Time.Clock (UTCTime (utctDay))
import Data.Time.Calendar (weekFirstDay, DayOfWeek (Monday))
import Data.Bifunctor (bimap)


withdraw :: Int -> Int -> KatipContextT ServerM ()
withdraw freqBase freq =
  forever $ do
    threadDelay $ freq * freqBase
    hasql <- fmap (^. hasqlDbPool) ask
    manager <- fmap (^.httpReqManager) ask
    go hasql manager
  where
    go hasql manager = do
      xs <- transactionM hasql $ statement fetchWithdrawals ()
      ys <- Async.forConcurrently xs $ \x -> do
        let body = uncurryT WithdrawalPaymentProviderRequest $ del1 x
        let mkResp = bimap ((x^._1,) . toS . show) (const (x^._1))
        let onFailure = pure . Left . show
        fmap mkResp $ Request.makePostReq @WithdrawalPaymentProviderRequest mempty manager [] body onFailure
      let (es, os) = partitionEithers ys
      for_ es $ \(ident, error) ->
        $(logTM) ErrorS $
          logStr @T.Text $
            $location <>
            ":withdraw: --> \ 
            \ withdrawal request failed to be sent, " <>
            toS (show @Int64 ident) <> ", error: " <> error
      void $ transactionM hasql $ statement updateWithdrawalStatus (Processing, os)

archive :: Int -> Int -> KatipContextT ServerM ()
archive freqBase freq =
  forever $ do
    threadDelay $ freq * freqBase
    hasql <- fmap (^. hasqlDbPool) ask
    go hasql
  where
    go hasql = do
      tm <-currentTime
      let day = utctDay tm
      let firstDay  = weekFirstDay Monday day
      when (day == firstDay) $
        void $ transactionM hasql $ statement archiveWallets ()