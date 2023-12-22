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
import Katip (KatipContextT)
import BuildInfo (location)
import Control.Monad (forever)
import Control.Concurrent.Lifted (threadDelay)
import Data.Foldable (for_)
import Data.Traversable (for)
import Database.Transaction (statement, transactionM)
import Control.Lens ((^.))
import Katip.Handler (hasqlDbPool, ask, httpReqManager)
import Data.Bifunctor (second)
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
        for xs $ \(instId, ys) -> do  
          statement Webhook.insert (instId, ys)
          fmap (const (instId, (map (getTransactionId &&& getTransactionStatus) ys))) $
            statement setPickedForDelivery $ 
              map getForwardedTransactionUUID ys
      for_ dbRes $ \o -> uncurry (N.makeS @"transaction_status") $ second (map (uncurry N.TransactionStatus)) o