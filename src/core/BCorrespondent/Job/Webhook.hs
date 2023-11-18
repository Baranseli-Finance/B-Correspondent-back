{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE RecordWildCards #-}

module BCorrespondent.Job.Webhook (go) where

import BCorrespondent.Job.Webhook.Institution.Tochka as Tochka
import BCorrespondent.Job.Webhook.Factory (Webhook (..))
import BCorrespondent.Statement.Webhook (fetch, markDelivered)
import BCorrespondent.Job.Utils (withElapsedTime)
import BCorrespondent.ServerM (ServerM)
import BuildInfo (location)
import Control.Monad (forever, join)
import Control.Concurrent.Lifted (threadDelay)
import Katip (KatipContextT, logTM, logStr, Severity (ErrorS))
import Database.Transaction (statement, transactionM)
import Katip.Handler (hasqlDbPool, httpReqManager, ask)
import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.Traversable (for)
import Control.Concurrent.Async.Lifted (forConcurrently)
import Data.Tuple.Extended (sel1, sel2, sel3, sel4, sel5)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (second)
import Data.Either (partitionEithers)
import Data.Text (Text)
import Data.String.Conv (toS)
import Katip.Monadic (askLoggerIO)
import Katip (Severity (InfoS), ls)


go :: Int -> KatipContextT ServerM ()
go freq = do
  let webhooks = [(1, Tochka.make)]
  forever $ do
    threadDelay $ freq * 1_000_000
    withElapsedTime ($location <> ":send") $ do 
      hasql <- fmap (^. hasqlDbPool) ask
      manager <- fmap (^. httpReqManager) ask
      xs <- transactionM hasql $ statement fetch ()
      xs' <- forConcurrently @[] xs $ \x -> do 
        let msg = "recipient " <>  show (sel2 x) <> " for webhook not found"
        fmap (join . maybe (Left msg) Right) $
          for (lookup (sel2 x) webhooks) $ \Webhook {..} -> do 
            mkLogger <- askLoggerIO  
            fmap (second (const (sel1 x))) $ liftIO $ send manager (mkLogger InfoS . ls) (sel4 x) (sel5 x) $ sel3 x
      let (es, os) = partitionEithers xs'
      transactionM hasql $ statement markDelivered os
      for_ es $ \error -> $(logTM) ErrorS $ logStr @Text $ $location <> ":go ---> " <> toS error
      
