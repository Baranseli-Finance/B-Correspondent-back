{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}

module BCorrespondent.Job.Transaction 
       (sendCompletedToTochkaBank, 
        forwardToElekse
       ) where

import BCorrespondent.Job.Utils (withElapsedTime)
import BCorrespondent.ServerM 
import Katip
import BuildInfo (location)
import Control.Monad (forever)
import Control.Concurrent.Lifted (threadDelay)

sendCompletedToTochkaBank :: KatipContextT ServerM ()
sendCompletedToTochkaBank = withElapsedTime $location $ forever $ threadDelay (10 * 1_000_000)

forwardToElekse :: KatipContextT ServerM ()
forwardToElekse = withElapsedTime $location $ forever $ threadDelay (10 * 1_000_000)