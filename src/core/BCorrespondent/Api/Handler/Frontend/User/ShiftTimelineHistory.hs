{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module BCorrespondent.Api.Handler.Frontend.User.ShiftTimelineHistory (handle) where

import BCorrespondent.Statement.Types
import BCorrespondent.Transport.Response (Response (Error))
import BCorrespondent.Api.Handler.Frontend.User.InitDashboard (transform)
import BCorrespondent.Statement.History (getHourShift)
import BCorrespondent.Api.Handler.Utils (withError)
import BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution)
import BCorrespondent.Transport.Error (asError)
import BCorrespondent.Transport.Model.Frontend (TimelineDirection (..), GapItemWrapper (..))
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM, ask, katipEnv, hasqlDbPool)
import Data.Text (Text)
import BuildInfo (location)
import Data.String (fromString)
import Katip (logTM, Severity (DebugS))
import Database.Transaction (transactionM, statement)
import Control.Lens ((^.))
import Data.Tuple.Extended (mapPolyT, consT, snocT)
import Data.Time.Calendar (weekFirstDay, weekLastDay, DayOfWeek (..))
import Control.Monad.Time (currentTime)
import Data.Time.Clock (UTCTime (utctDay))
import qualified Data.Time.Calendar.OrdinalDate as D (Day)

handle 
  :: Auth.AuthenticatedUser 'Auth.Reader  
  -> Int
  -> Int
  -> Int
  -> TimelineDirection
  -> Int
  -> Int
  -> KatipHandlerM (Response GapItemWrapper)
handle user y m d direction institutionIdent hour
  | 0 > hour || hour > 24 = 
    pure $ Error (Just 400) $ asError @Text "hour must lie between 0 and 23, 0 < hour || hour > 23"
  | (direction == Forward && hour + 1 > 24) || 
    (direction == Backward && hour - 1 < 0) = 
      let msg = 
           "hour must lie between 0 and 23,\
           \ direction == Forward && hour + 1 > 24 \
           \ or direction == Backward && hour - 1 < 0" 
      in pure $ Error (Just 400) $ asError @Text msg
  | otherwise = 
      checkInstitution user $ \(_, yourInstitutionIdent) ->
        if institutionIdent /= fromIntegral yourInstitutionIdent
        then pure $ Error (Just 403) $ asError @Text "you should be granted the additional permission to monitor the second part"
        else do
          isCurrent <- checkIfCurrentPeriod y m d
          hasql <- fmap (^. katipEnv . hasqlDbPool) ask
          let from | direction == Forward = hour
                  | otherwise = hour - 1
          let to | direction == Forward = hour + 1
                | otherwise = hour
          let params =
                snocT isCurrent $
                consT yourInstitutionIdent $ 
                consT (Year (fromIntegral y)) $
                consT (Month (fromIntegral m)) $
                consT (Day (fromIntegral d)) $
                mapPolyT (Hour . fromIntegral) (from, to)
          $(logTM) DebugS $ fromString $ $location <> " params ---> " <> show params
          fmap (`withError`(GapItemWrapper . transform)) $ transactionM hasql $ statement getHourShift params

checkIfCurrentPeriod y m d = do
  let addZero x = if x < 10 then "0" <> show x else show x
  let givenDay = read @D.Day $ show y <> "-" <> addZero m <> "-" <> addZero d
  let start = weekFirstDay Monday givenDay
  let end = weekLastDay Monday givenDay
  day <- fmap utctDay currentTime
  return $ day >= start && day <= end