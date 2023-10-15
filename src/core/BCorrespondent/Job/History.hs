{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE RecordWildCards #-}

module BCorrespondent.Job.History (refreshMV) where

import BCorrespondent.Job.Utils (withElapsedTime)
import BCorrespondent.ServerM (ServerM)
import Katip
import BuildInfo (location)
import Control.Monad (forever)
import Control.Concurrent.Lifted (threadDelay)

refreshMV :: Int -> KatipContextT ServerM ()
refreshMV freq =
  forever $ do 
    threadDelay $ freq * 1_000_000
    withElapsedTime ($location <> ":refreshMV") $
      undefined