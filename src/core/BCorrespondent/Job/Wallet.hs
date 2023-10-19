{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE RecordWildCards #-}

module BCorrespondent.Job.Wallet (withdraw) where

import BCorrespondent.Job.Utils (withElapsedTime)
import BCorrespondent.ServerM (ServerM)
import Katip
import BuildInfo (location)
import Control.Monad (forever)
import Control.Concurrent.Lifted (threadDelay)

withdraw :: Int -> KatipContextT ServerM ()
withdraw freq =
  forever $ do
    threadDelay $ freq * 1_000_000
    withElapsedTime ($location <> ":withdraw") undefined