{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module BCorrespondent.Api.Handler.Frontend.User.FetchTimeline (handle) where

import BCorrespondent.Api.Handler.Frontend.User.InitDashboard (transform)
import BCorrespondent.Statement.Dashboard (get1HourTimeline)
import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Transport.Model.Frontend 
       (TimelineDirection (..), GapItem, GapItemTime (..))
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Api.Handler.Utils (withError)
import BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution)
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, ask)
import Control.Monad.Time (currentTime)
import Data.Time.Clock (utctDay, UTCTime (..))
import Data.Time.Calendar.OrdinalDate (toOrdinalDate, fromOrdinalDate)
import Database.Transaction (transactionM, statement)
import Control.Lens ((^.))
import Katip (logTM, Severity (DebugS))
import Data.String (fromString)
import BuildInfo (location)

handle :: Auth.AuthenticatedUser 'Auth.Reader -> TimelineDirection -> GapItemTime -> KatipHandlerM (Response [GapItem])
handle user direction GapItemTime {gapItemTimeHour=hour, gapItemTimeMin=min} = 
  checkInstitution user $ \(_, ident) -> do 
    date@(year, day) <- fmap (toOrdinalDate . utctDay) currentTime
    let params
          | direction == Backward = 
              let start =
                    UTCTime 
                    (uncurry fromOrdinalDate date) $
                    fromIntegral $
                     (if hour == 0 then 0 else hour - 1) * 3600 + 
                     (if hour == 0 then 0 else min) * 60
                  end = 
                    UTCTime 
                    (uncurry fromOrdinalDate date) $ 
                    if hour == 0 then 3600 
                    else fromIntegral $ hour * 3600 + min * 60
              in (start, end, ident)   
          | otherwise = 
              let start = 
                    UTCTime 
                    (uncurry fromOrdinalDate date) $
                    if hour == 23 then 23 * 3600
                    else fromIntegral $ hour * 3600 + min * 60
                  end = 
                    UTCTime 
                    (uncurry fromOrdinalDate (year, if hour == 23 then day + 1 else day)) $
                    fromIntegral $ 
                    (if hour == 23 then 0 else hour + 1) * 3600 + 
                    (if hour == 23 then 0 else min) * 60
              in (start, end, ident)
    $(logTM) DebugS $ fromString $ $location <> " params ---> " <> show params        
    hasql <- fmap (^. katipEnv . hasqlDbPool) ask           
    dbResp <- transactionM hasql $ statement get1HourTimeline params
    pure $ withError dbResp transform