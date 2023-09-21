{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TypeApplications #-}

module BCorrespondent.Job.Transaction (sendToTochkaBank) where

import BCorrespondent.Statement.Transaction (getTransactionsToBeSent, insertFailedTransactions)
import BCorrespondent.Job.Utils (withElapsedTime)
import BCorrespondent.ServerM 
import Katip
import BuildInfo (location)
import Control.Monad (forever)
import Control.Concurrent.Lifted (threadDelay)
import Katip.Handler
import Control.Lens ((^.))
import Database.Transaction (statement, transactionM)
import Data.Traversable (for)
import Data.Foldable (for_)
import Data.Either (partitionEithers)
import qualified Data.Text as T

sendToTochkaBank :: KatipContextT ServerM ()
sendToTochkaBank = 
  forever $
    withElapsedTime ($location <> ":sendToTochkaBank") $ do
      threadDelay $ 10 * 1_000_000
      hasql <- fmap (^. hasqlDbPool) ask
      xs <- transactionM hasql $ statement getTransactionsToBeSent ()
      ys <- for xs $ \_ -> undefined
      let es = fst $ partitionEithers ys
      for_ es $ \_ ->
        $(logTM) ErrorS $ logStr @T.Text $ $location <> ":sendToTochkaBank: --> error"
      transactionM hasql $ statement insertFailedTransactions es