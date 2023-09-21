{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TypeApplications #-}

module BCorrespondent.Job.Transaction (sendToTochkaBank) where

import BCorrespondent.Statement.Transaction 
       (getTransactionsToBeSent, insertSentTransactions, insertFailedTransactions)
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
import Data.String.Conv (toS)

sendToTochkaBank :: KatipContextT ServerM ()
sendToTochkaBank = 
  forever $ do 
    threadDelay $ 10 * 1_000_000   
    withElapsedTime ($location <> ":sendToTochkaBank") $ do
      hasql <- fmap (^. hasqlDbPool) ask
      res <- transactionM hasql $ statement getTransactionsToBeSent ()
      case res of 
        Right xs -> do
          ys <- for xs $ \_ -> undefined
          let (es, os) = partitionEithers ys
          for_ es $ \ident ->
            $(logTM) ErrorS $
              logStr @T.Text $
                $location <> ":sendToTochkaBank: --> transaction details failed to be sent, " <>
                toS (show ident)
          transactionM hasql $ do 
            statement insertFailedTransactions es
            statement insertSentTransactions os
        Left err -> $(logTM) CriticalS $ logStr @T.Text $ $location <> ":sendToTochkaBank: decode error ---> " <> toS err