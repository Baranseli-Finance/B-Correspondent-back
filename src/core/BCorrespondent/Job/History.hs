{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE RecordWildCards #-}

module BCorrespondent.Job.History (refreshMV) where

import qualified BCorrespondent.Statement.History as S (refreshMV)
import BCorrespondent.ServerM (ServerM)
import Katip
import Control.Concurrent.Lifted (threadDelay)
import Database.Transaction (transactionM, statement)
import Katip.Handler (hasqlDbPool, ask)
import Control.Lens ((^.))
import Control.Monad.Time (currentTime)
import Data.Time.Clock (utctDay)
import Control.Monad (when, forever)
import Data.Time.Calendar (weekFirstDay, DayOfWeek (Monday))


refreshMV :: Int -> Int -> KatipContextT ServerM ()
refreshMV freqBase freq =
  forever $ do
    threadDelay $ freq * freqBase
    tm <- currentTime
    let day = utctDay tm
    let firstDay = weekFirstDay Monday day
    when (day == firstDay) $
      fmap (^. hasqlDbPool) ask >>= (`transactionM` (statement S.refreshMV ()))