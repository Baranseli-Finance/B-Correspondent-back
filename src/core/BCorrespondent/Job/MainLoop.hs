{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}

module BCorrespondent.Job.MainLoop (reload) where

import BCorrespondent.Job.Utils (withElapsedTime, forever)
import BCorrespondent.ServerM (ServerM, ServerException (Reload))
import Katip (KatipContextT)
import BuildInfo (location)
import Control.Concurrent.Lifted (threadDelay)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Control.Monad.Catch (throwM)

reload :: Int -> Int -> KatipContextT ServerM ()
reload freq start  =
  forever $ do
    threadDelay $ freq * 1_000_000
    withElapsedTime ($location <> ":reload") $ do 
      curr <- fmap (round . utctDayTime) $ liftIO getCurrentTime
      -- every 6 hours
      when (curr - start > 21_600) $ throwM Reload