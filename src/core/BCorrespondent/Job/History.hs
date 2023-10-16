{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE RecordWildCards #-}

module BCorrespondent.Job.History (refreshMV) where

import qualified BCorrespondent.Statement.History as S (getLastRefreshTm, refreshMV)
import BCorrespondent.Job.Utils (withElapsedTime)
import BCorrespondent.ServerM (ServerM)
import Katip
import BuildInfo (location)
import Control.Monad (forever)
import Control.Concurrent.Lifted (threadDelay)
import Database.Transaction (transactionM, statement)
import Katip.Handler (hasqlDbPool, ask)
import Control.Lens ((^.))
import Control.Monad.Time (currentTime)
import Data.Time.Clock (utctDay)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Control.Monad (when)

refreshMV :: Int -> KatipContextT ServerM ()
refreshMV freq =
  forever $ do
    threadDelay $ freq * 1_000_000
    withElapsedTime ($location <> ":refreshMV") $ do
      currDoY <- fmap (snd . toOrdinalDate . utctDay) currentTime
      hasql <- fmap (^. hasqlDbPool) ask
      mvDoY <- transactionM hasql $ statement S.getLastRefreshTm ()
      when (currDoY > mvDoY) $ transactionM hasql $ statement S.refreshMV ()