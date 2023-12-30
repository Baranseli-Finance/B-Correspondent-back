{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}

module BCorrespondent.Job.Cache (removeExpiredItems) where

-- import BCorrespondent.Job.Utils (withElapsedTime)
import BCorrespondent.ServerM (ServerM)
import Katip (KatipContextT)
-- import BuildInfo (location)
import Control.Concurrent.Lifted (threadDelay)
-- import qualified Control.Monad.State.Class as ST
-- import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
-- import Cache (clean)

removeExpiredItems :: Int -> KatipContextT ServerM ()
removeExpiredItems freq = do
  threadDelay $ freq * 1_000_000
  liftIO $ print "rubbish"
  -- withElapsedTime ($location <> ":removeExpiredItems") $
  -- fmap cache (lift ST.get) >>= clean
  removeExpiredItems freq