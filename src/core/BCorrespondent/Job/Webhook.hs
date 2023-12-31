{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE TupleSections #-}

module BCorrespondent.Job.Webhook (run) where

import qualified BCorrespondent.Institution.Webhook as W
import BCorrespondent.Institution.Webhook.Factory (Webhook (..))
import BCorrespondent.Statement.Webhook (fetch, markDelivered, addError)
import BCorrespondent.ServerM (ServerM)
import BuildInfo (location)
import Control.Monad (join, forever)
import Control.Concurrent.Lifted (threadDelay)
import Katip (KatipContextT, logTM, logStr, Severity (ErrorS))
import Database.Transaction (statement, transactionM)
import Katip.Handler (hasqlDbPool, httpReqManager, ask)
import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.Traversable (for)
import Control.Concurrent.Async.Lifted (forConcurrently)
import Data.Tuple.Extended (sel1, sel2, sel3, sel4, sel5)
import Data.Bifunctor (bimap)
import Data.Either (partitionEithers)
import Data.Text (Text)
import Data.String.Conv (toS)


run :: Int -> KatipContextT ServerM ()
run freq =
  forever $ do
    threadDelay $ freq * 1_000_000
    hasql <- fmap (^. hasqlDbPool) ask
    manager <- fmap (^. httpReqManager) ask
    xs <- transactionM hasql $ statement fetch ()
    xs' <- forConcurrently @[] xs $ \x -> do 
      let msg = "recipient " <>  toS (show (sel2 x)) <> " for webhook not found"
      fmap (join . maybe (Left (sel1 x, msg)) Right) $
        for (lookup (sel2 x) W.webhooks) $ \Webhook {..} ->
          fmap (bimap ((sel1 x,) . toS) (const (sel1 x))) $ send manager (sel4 x) (sel5 x) $ sel3 x
    let (es, os) = partitionEithers xs'
    transactionM hasql $ statement markDelivered os >> statement addError es
    for_ es $ \(_, error) -> $(logTM) ErrorS $ logStr @Text $ $location <> ":run ---> " <> toS error
      
