{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}

module BCorrespondent.Job.Transaction 
       (sendCompletedToTochkaBank, 
        forwardToElekse
       ) where

import BCorrespondent.Job.Utils (withElapsedTime)
import BCorrespondent.ServerM 
import Katip
import BuildInfo (location)

sendCompletedToTochkaBank :: KatipContextT ServerM ()
sendCompletedToTochkaBank = withElapsedTime $location $ undefined

forwardToElekse :: KatipContextT ServerM ()
forwardToElekse = withElapsedTime $location $ undefined