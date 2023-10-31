{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE TupleSections #-}

module BCorrespondent.Job.Transaction (forwardToInitiator) where

import BCorrespondent.Statement.Transaction 
       (getTransactionsToBeSent, insertSentTransactions, insertFailedTransactions)
import BCorrespondent.Job.Utils (withElapsedTime)
import BCorrespondent.ServerM
import BCorrespondent.Transport.Model.Transaction (TransactionToInitiator, TransactionId (..))
import Katip
import BuildInfo (location)
import Control.Monad (forever)
import Control.Concurrent.Lifted (threadDelay)
import Katip.Handler
import Control.Lens ((^.))
import Database.Transaction (statement, transactionM)
import Data.Foldable (for_)
import Data.Either (partitionEithers)
import qualified Data.Text as T
import Data.String.Conv (toS)
import Data.Aeson.WithField (WithField (..))
import qualified Control.Concurrent.Async.Lifted as Async
import qualified Request as Request
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)


forwardToInitiator :: Int -> KatipContextT ServerM ()
forwardToInitiator freq = 
  forever $ do 
    threadDelay $ freq * 1_000_000   
    withElapsedTime ($location <> ":forwardToInitiator") $ do
      hasql <- fmap (^. hasqlDbPool) ask
      res <- transactionM hasql $ statement getTransactionsToBeSent ()
      case res of 
        Right xs -> do
          manager <- fmap (^.httpReqManager) ask
          ys <- Async.forConcurrently xs $ 
            \(WithField ident transaction) -> do  
                let req = Left $ Just $ transaction
                let mkResp = bimap ((coerce ident,) . toS . show) (const ident)
                let onFailure = pure . Left . show
                fmap mkResp $ Request.safeMake @TransactionToInitiator "https://test.com" manager [] Request.methodPost req onFailure
          let (es, os) = partitionEithers ys
          for_ es $ \(ident, error) ->
            $(logTM) ErrorS $
              logStr @T.Text $
                $location <>
                ":forwardToInitiator: --> \ 
                \ transaction details failed to be sent, " <>
                toS (show ident) <> ", error: " <> error
          transactionM hasql $ do 
            statement insertFailedTransactions es
            statement insertSentTransactions os
        Left err -> $(logTM) CriticalS $ logStr @T.Text $ $location <> ":forwardToInitiator: decode error ---> " <> toS err