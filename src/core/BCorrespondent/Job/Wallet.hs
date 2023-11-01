{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE TupleSections #-}

module BCorrespondent.Job.Wallet (withdraw, archive) where

import BCorrespondent.Transport.Model.Institution 
       (WithdrawalPaymentProviderRequest (..), WithdrawalStatus (Processing))
import BCorrespondent.Statement.Institution 
       (fetchWithdrawals, updateWithdrawalStatus, refreshWalletMV)    
import BCorrespondent.Job.Utils (withElapsedTime)
import BCorrespondent.ServerM (ServerM)
import Katip
import BuildInfo (location)
import Control.Monad (forever)
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
import Control.Monad (void, when)
import Control.Monad.Time (currentTime)
import Data.Time.Clock (UTCTime (utctDay))
import Data.Time.Calendar (weekFirstDay, DayOfWeek (Monday))
import Data.Bifunctor (bimap)


withdraw :: Int -> KatipContextT ServerM ()
withdraw freq =
  forever $ do
    threadDelay $ freq * 1_000_000
    withElapsedTime ($location <> ":withdraw") $ do 
      hasql <- fmap (^. hasqlDbPool) ask
      manager <- fmap (^.httpReqManager) ask
      xs <- transactionM hasql $ statement fetchWithdrawals ()
      ys <- Async.forConcurrently xs $ \x -> do
        let body = uncurryT WithdrawalPaymentProviderRequest $ del1 x
        let mkResp = bimap ((x^._1,) . toS . show) (const (x^._1))
        let onFailure = pure . Left . show
        fmap mkResp $ Request.makePostReq @WithdrawalPaymentProviderRequest "https://test.com" manager [] body onFailure
      let (es, os) = partitionEithers ys
      for_ es $ \(ident, error) ->
        $(logTM) ErrorS $
          logStr @T.Text $
            $location <>
            ":withdraw: --> \ 
            \ withdrawal request failed to be sent, " <>
            toS (show @Int64 ident) <> ", error: " <> error
      void $ transactionM hasql $ statement updateWithdrawalStatus (Processing, os)

archive :: Int -> KatipContextT ServerM ()
archive freq =
  forever $ do
    threadDelay $ freq * 1_000_000
    withElapsedTime ($location <> ":archive") $ do 
      tm <-currentTime
      let day = utctDay tm
      let firstDay  = weekFirstDay Monday day
      when (day == firstDay) $ do 
        hasql <- fmap (^. hasqlDbPool) ask
        void $ transactionM hasql $ statement refreshWalletMV ()
