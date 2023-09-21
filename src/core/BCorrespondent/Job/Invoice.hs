{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TypeApplications #-}

module BCorrespondent.Job.Invoice (forwardToElekse) where

import BCorrespondent.Statement.Invoice (getInvoicesToBeSent, insertFailedInvoices, updateStatus, Status (ForwardedToElekse))
import BCorrespondent.Job.Utils (withElapsedTime)
import BCorrespondent.ServerM 
import Katip
import BuildInfo (location)
import Control.Monad (forever)
import Control.Concurrent.Lifted (threadDelay)
import Katip.Handler
import Control.Lens ((^.), (<&>))
import Database.Transaction (statement, transactionM)
import Data.Traversable (for)
import Data.Foldable (for_)
import Data.Either (partitionEithers)
import qualified Data.Text as T
import Data.String.Conv (toS)

forwardToElekse :: Int -> KatipContextT ServerM ()
forwardToElekse freq =
  forever $ do 
    threadDelay $ freq * 1_000_000
    withElapsedTime ($location <> ":forwardToElekse") $ do
      hasql <- fmap (^. hasqlDbPool) ask
      res <- transactionM hasql $ statement getInvoicesToBeSent ()
      case res of
        Right xs -> do
          ys <- for xs $ \_ -> undefined
          let (es, os) = partitionEithers ys
          for_ es $ \ident ->
            $(logTM) ErrorS $ 
            logStr @T.Text $ 
              $location <>
              ":forwardToElekse: --> invoice failed to be sent, " <> 
              toS (show ident)
          transactionM hasql $ do 
            statement insertFailedInvoices es
            statement updateStatus $ os <&> \x -> (x, ForwardedToElekse)
        Left err -> $(logTM) CriticalS $ logStr @T.Text $ $location <> ":forwardToElekse: decode error ---> " <> toS err