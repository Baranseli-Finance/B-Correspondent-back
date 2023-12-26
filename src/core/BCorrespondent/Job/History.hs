{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE RecordWildCards #-}

module BCorrespondent.Job.History (refreshMV) where

import qualified BCorrespondent.Statement.History as S (refreshMV)
import BCorrespondent.Job.Utils (withElapsedTime, forever)
import BCorrespondent.ServerM (ServerM)
import Katip
import BuildInfo (location)
import Control.Concurrent.Lifted (threadDelay)
import Database.Transaction (transactionM, statement)
import Katip.Handler (hasqlDbPool, ask)
import Control.Lens ((^.))
import Control.Monad.Time (currentTime)
import Data.Time.Clock (utctDay)
import Control.Monad (when)
import Data.Time.Calendar (weekFirstDay, DayOfWeek (Monday))

refreshMV :: Int -> KatipContextT ServerM ()
refreshMV freq =
  forever $ do
    threadDelay $ freq * 1_000_000
    withElapsedTime ($location <> ":refreshMV") $ do
      tm <-currentTime
      let day = utctDay tm
      let firstDay = weekFirstDay Monday day
      when (day == firstDay) $ do
        hasql <- fmap (^. hasqlDbPool) ask
        transactionM hasql $ statement S.refreshMV ()