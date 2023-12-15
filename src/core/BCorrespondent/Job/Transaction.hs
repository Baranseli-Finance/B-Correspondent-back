{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE TypeApplications #-}

module BCorrespondent.Job.Transaction (abort) where

import BCorrespondent.Statement.Transaction (fetchAbortedTransaction)
import BCorrespondent.Transport.Model.Transaction (AbortedTransactionRequest (..))
import BCorrespondent.Job.Utils (withElapsedTime)
import BCorrespondent.ServerM (ServerM)
import Katip (KatipContextT, logTM, Severity (ErrorS), logStr)
import BuildInfo (location)
import Control.Monad (forever)
import Control.Concurrent.Lifted (threadDelay)
import qualified Control.Concurrent.Async.Lifted as Async
import qualified Request as Request
import Data.Text (Text)
import Data.Int (Int64)
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.String.Conv (toS)
import Database.Transaction (statement, transactionM)
import Control.Lens ((^.))
import Katip.Handler (hasqlDbPool, ask, httpReqManager)
import Data.Bifunctor (bimap)


abort :: Int -> KatipContextT ServerM ()
abort freq =
  forever $ do
    threadDelay $ freq * 1_000_000
    withElapsedTime ($location <> ":abort") $ do 
      hasql <- fmap (^. hasqlDbPool) ask
      manager <- fmap (^.httpReqManager) ask
      xs <- transactionM hasql $ statement fetchAbortedTransaction ()
      ys <- Async.forConcurrently xs $ \x -> do
        let body = AbortedTransactionRequest $ snd x
        let mkResp = bimap ((fst x,) . toS . show) (const (fst x))
        let onFailure = pure . Left . show
        fmap mkResp $ Request.makePostReq @AbortedTransactionRequest mempty manager [] body onFailure
      let (es, _) = partitionEithers ys
      for_ es $ \(ident, error) ->
        $(logTM) ErrorS $
          logStr @Text $
            $location <>
            ":abort: --> \
            \ abort transaction request failed to be sent, invoice " <>
            toS (show @Int64 ident) <> ", error: " <> error