{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE NamedFieldPuns #-}
{-#LANGUAGE ScopedTypeVariables #-}

module BCorrespondent.Job.Transaction (abort) where

import qualified BCorrespondent.Institution.Query as Q
import BCorrespondent.Statement.Transaction (fetchAbortedTransaction)
import BCorrespondent.Transport.Model.Transaction (AbortedTransactionRequest (..))
import qualified BCorrespondent.Institution.Query.AbortedTransaction as Q
import BCorrespondent.Statement.Invoice (QueryCredentials (..))
import BCorrespondent.Job.Utils (withElapsedTime)
import BCorrespondent.ServerM (ServerM)
import Katip (KatipContextT, logTM, Severity (ErrorS), logStr)
import BuildInfo (location)
import Control.Monad (forever, join)
import Control.Concurrent.Lifted (threadDelay)
import qualified Control.Concurrent.Async.Lifted as Async
import Data.Text (Text)
import Data.Int (Int64)
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.Traversable (for)
import Data.String.Conv (toS)
import Database.Transaction (statement, transactionM)
import Control.Lens ((^.))
import Katip.Handler (hasqlDbPool, ask, httpReqManager)
import Data.Bifunctor (bimap)
import Data.Tuple.Extended (sel1, sel2, sel3)
import Data.Maybe (fromMaybe)


abort :: Int -> KatipContextT ServerM ()
abort freq =
  forever $ do
    threadDelay $ freq * 1_000_000
    withElapsedTime ($location <> ":abort") $ do 
      hasql <- fmap (^. hasqlDbPool) ask
      manager <- fmap (^.httpReqManager) ask
      xs <- transactionM hasql $ statement fetchAbortedTransaction ()
      ys <- Async.forConcurrently xs $ \x -> do
        let body = AbortedTransactionRequest $ sel3 x
        let mkResp = bimap ((sel1 x,) . toS) (const (sel1 x))
        fmap (mkResp . join) $ 
          for (sel2 x) $ \cred ->
            fmap (fromMaybe (Left "payment provider credentials aren't set")) $ 
              for cred $ \QueryCredentials {..} ->
                fmap (join . fromMaybe (Left "provider not found")) $
                  for (lookup provider Q.queries) $ 
                    \(Q.Query {fetchToken, makeRequest}) -> do
                      authRes <- fetchToken manager login password
                      for authRes $ \token -> makeRequest @() manager Q.path token body
      let (es, _) = partitionEithers ys
      for_ es $ \(ident, error) ->
        $(logTM) ErrorS $
          logStr @Text $
            $location <>
            ":abort: --> \
            \ abort transaction request failed to be sent, invoice " <>
            toS (show @Int64 ident) <> ", error: " <> error