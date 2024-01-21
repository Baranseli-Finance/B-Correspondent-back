{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}

module BCorrespondent.Job.Cache (removeExpiredItems) where

import BCorrespondent.ServerM (ServerM, cache)
import Katip (KatipContextT)
import Control.Concurrent.Lifted (threadDelay)
import qualified Control.Monad.State.Class as ST
import Control.Monad.Trans.Class (lift)
import Cache (clean)
import Control.Monad (forever)
import qualified Control.Concurrent.Async.Lifted as Async

removeExpiredItems :: Int -> Int -> KatipContextT ServerM ()
removeExpiredItems freqBase freq =
  forever $ do 
    threadDelay $ freq * freqBase
    Async.async (fmap cache (lift ST.get) >>= clean) >>= Async.wait