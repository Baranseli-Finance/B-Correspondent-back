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

forwardToElekse :: KatipContextT ServerM ()
forwardToElekse =
  forever $
    withElapsedTime ($location <> ":forwardToElekse") $ do
      threadDelay $ 10 * 1_000_000
      hasql <- fmap (^. hasqlDbPool) ask
      xs <- transactionM hasql $ statement getInvoicesToBeSent ()
      ys <- for xs $ \_ -> undefined
      let (es, os) = partitionEithers ys
      for_ es $ \ident ->
        $(logTM) ErrorS $ 
          logStr @T.Text $ 
            $location <>
            ":forwardToElekse: --> invoice failed to be sent, " <> 
            toS (show ident) 
      transactionM hasql $ statement insertFailedInvoices es
      transactionM hasql $ statement updateStatus $ os <&> \x -> (x, ForwardedToElekse)