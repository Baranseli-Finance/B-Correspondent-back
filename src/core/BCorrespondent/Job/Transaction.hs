{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE NamedFieldPuns #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE DataKinds #-}

module BCorrespondent.Job.Transaction (forward) where


import BCorrespondent.Statement.Transaction 
       (fetchForwardedTransaction, setPickedForDelivery, 
        getForwardedTransactionUUID, getTransactionId, getTransactionStatus)
import qualified BCorrespondent.Statement.Webhook as Webhook
import qualified BCorrespondent.Notification as N
import BCorrespondent.Job.Utils (withElapsedTime)
import BCorrespondent.ServerM (ServerM)
import Katip (KatipContextT, logTM, Severity (ErrorS), logStr)
import BuildInfo (location)
import Control.Monad (forever)
import Control.Concurrent.Lifted (threadDelay)
import Data.Text (Text)
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.Traversable (for)
import Data.String.Conv (toS)
import Database.Transaction (statement, transactionM)
import Control.Lens ((^.))
import Katip.Handler (hasqlDbPool, ask, httpReqManager)
import Data.Bifunctor (bimap, second)
import Data.Either.Combinators (swapEither)
import Control.Arrow ((&&&))


forward :: Int -> KatipContextT ServerM ()
forward freq =
  forever $ do
    threadDelay $ freq * 1_000_000
    withElapsedTime ($location <> ":forward") $ do 
      hasql <- fmap (^. hasqlDbPool) ask
      manager <- fmap (^.httpReqManager) ask
      dbRes <- transactionM hasql $ do 
        xs <- statement fetchForwardedTransaction ()
        for xs $ \(instId, yse) -> 
          fmap (bimap (instId,) (instId,) . swapEither) $ 
            for yse $ \ys -> do 
              statement Webhook.insert (instId, ys)
              fmap (const (map (getTransactionId &&& getTransactionStatus) ys)) $
                statement setPickedForDelivery $ 
                  map getForwardedTransactionUUID ys 
      let (os, es) = partitionEithers dbRes
      for_ es $ \(ident, error) ->
        let msg = 
              $location <> 
              ":forward: transactions forwarding error " <> 
              toS error <>
              ", institution " <> 
              toS (show ident)
        in $(logTM) ErrorS $ logStr @Text msg
      for_ os $ \o -> uncurry (N.makeS @"transaction_status") $ second (map (uncurry N.TransactionStatus)) o