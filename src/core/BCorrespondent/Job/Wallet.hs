{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE RecordWildCards #-}

module BCorrespondent.Job.Wallet (withdraw) where

import BCorrespondent.Transport.Model.Institution 
       (WithdrawalPaymentProviderRequest (..), WithdrawalStatus (Processing))
import BCorrespondent.Statement.Institution 
       (fetchWithdrawals, updateWithdrawalStatus)    
import BCorrespondent.Job.Utils (withElapsedTime)
import BCorrespondent.ServerM (ServerM)
import Katip
import BuildInfo (location)
import Control.Monad (forever)
import Control.Concurrent.Lifted (threadDelay)
import Control.Lens ((^.))
import Database.Transaction (statement, transactionM)
import Katip.Handler
import qualified Control.Concurrent.Async.Lifted as Async
import qualified Request as Request
import Data.Tuple.Extended (del1)
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.String.Conv (toS)
import qualified Data.Text as T
import Data.Int (Int64)
import Control.Monad (void)

withdraw :: Int -> KatipContextT ServerM ()
withdraw freq =
  forever $ do
    threadDelay $ freq * 1_000_000
    withElapsedTime ($location <> ":withdraw") $ do 
      hasql <- fmap (^. hasqlDbPool) ask
      manager <- fmap (^.httpReqManager) ask
      xs <- transactionM hasql $ statement fetchWithdrawals ()
      ys <- Async.forConcurrently xs $ \x -> do
        let req = Left $ Just $ uncurry WithdrawalPaymentProviderRequest $ del1 x
        _ <- Request.make @WithdrawalPaymentProviderRequest undefined manager [] Request.methodPost req
        undefined
      let (es, os) = partitionEithers ys
      for_ es $ \ident ->
        $(logTM) ErrorS $
          logStr @T.Text $
            $location <>
            ":withdraw: --> \ 
            \ withdrawal request failed to be sent, " <>
            toS (show @Int64 ident)
      void $ transactionM hasql $ statement updateWithdrawalStatus (Processing, os)
