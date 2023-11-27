{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE TupleSections #-}

module BCorrespondent.Job.Transaction (forwardToInitiator) where

import BCorrespondent.Statement.Transaction 
       (getTransactionsToBeSent)
import BCorrespondent.Job.Utils (withElapsedTime)
import BCorrespondent.ServerM
import Katip
import BuildInfo (location)
import Control.Monad (forever)
import Control.Concurrent.Lifted (threadDelay)
import Katip.Handler
import Control.Lens ((^.))
import Database.Transaction (statement, transactionM)
import qualified Data.Text as T
import Data.String.Conv (toS)


forwardToInitiator :: Int -> KatipContextT ServerM ()
forwardToInitiator freq =
  forever $ do 
    threadDelay $ freq * 1_000_000   
    withElapsedTime ($location <> ":forwardToInitiator") $ do
      hasql <- fmap (^. hasqlDbPool) ask
      res <- transactionM hasql $ statement getTransactionsToBeSent ()
      case res of
        Right _ -> return ()
        Left err -> $(logTM) CriticalS $ logStr @T.Text $ $location <> ":forwardToInitiator: decode error ---> " <> toS err