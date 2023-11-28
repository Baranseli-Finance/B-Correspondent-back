{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE NamedFieldPuns #-}

module BCorrespondent.Job.Cache (removeExpiredItems) where

import BCorrespondent.Job.Utils (withElapsedTime)
import BCorrespondent.ServerM (ServerM, ServerState (..))
import Katip (KatipContextT)
import BuildInfo (location)
import Control.Monad (forever)
import Control.Concurrent.Lifted (threadDelay)
import qualified Control.Monad.State.Class as ST
import Control.Monad.Trans.Class (lift)
import Cache (clean)

removeExpiredItems :: Int -> KatipContextT ServerM ()
removeExpiredItems freq =
  forever $ do
    threadDelay $ freq * 1_000_000
    withElapsedTime ($location <> ":removeExpiredItems") $ do 
      ServerState {cache} <- lift ST.get
      clean cache