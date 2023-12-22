{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE NamedFieldPuns #-}
{-#LANGUAGE ScopedTypeVariables #-}

module BCorrespondent.Job.Transaction (forward) where


import BCorrespondent.Statement.Transaction (fetchForwardedTransaction, setPickedForDelivery, getForwardedTransactionUUID)
import qualified BCorrespondent.Statement.Webhook as Webhook
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
import Data.Bifunctor (first)


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
          fmap (first (instId,)) $ 
            for yse $ \ys -> do 
              statement Webhook.insert (instId, ys)
              statement setPickedForDelivery $ 
                map getForwardedTransactionUUID ys 
      let es = fst $ partitionEithers dbRes
      for_ es $ \(ident, error) ->
        let msg = 
              $location <> 
              ":forward: transactions forwarding error " <> 
              toS error <>
              ", institution " <> 
              toS (show ident)
        in $(logTM) ErrorS $ logStr @Text msg