{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE RecordWildCards #-}

module BCorrespondent.Api.Handler.Frontend.User.InitDashboard (handle, mkStatus, transform) where

import BCorrespondent.Transport.Model.Frontend 
       (DailyBalanceSheet (..), 
        GapItem (..), 
        GapItemTime (..), 
        GapItemUnit (..),
        GapItemUnitStatus (..),
        InitDashboard (..)
       )
import BCorrespondent.Statement.Dashboard (getDailyBalanceSheet, HourTimeline (..))
import qualified BCorrespondent.Statement.Dashboard as S (DailyBalanceSheet (..))
import BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution)
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Statement.Invoice  (Status (..))
import BCorrespondent.Api.Handler.Utils (withError)
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, ask)
import Control.Monad.Time (currentTime)
import Data.Time.Clock (UTCTime (..), addUTCTime)
import Data.Time.LocalTime (todMin, timeToTimeOfDay)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate, DayOfYear, Year, fromOrdinalDate)
import Database.Transaction (transactionM, statement)
import Control.Lens ((^.))
import GHC.Exts (groupWith, the)
import Data.Functor ((<&>))

handle :: Auth.AuthenticatedUser 'Auth.Reader -> KatipHandlerM (Response InitDashboard)
handle user = 
  checkInstitution user $ \(_, ident) -> do
    tm <- currentTime
    let day = toOrdinalDate $ utctDay tm
    let shift = (mod (60 - todMin (timeToTimeOfDay (utctDayTime tm))) 5) * 60
    let upperBound = addUTCTime (fromRational (fromIntegral shift)) tm
    let lowerBoundTmp = addUTCTime (-3600) upperBound
    let lowerBound = mkLowerBound lowerBoundTmp day
    hasql <- fmap (^. katipEnv . hasqlDbPool) ask
    let mkResp dbResp =
          withError dbResp $ \S.DailyBalanceSheet {..} ->
            InitDashboard 
            { initDashboardDailyBalanceSheet = 
                DailyBalanceSheet 
                dailyBalanceSheetInstitution $ 
                transform dailyBalanceSheetTimeline 
            }
    fmap mkResp $ transactionM hasql $ statement getDailyBalanceSheet (lowerBound, upperBound, ident)

transform :: [HourTimeline] -> [GapItem]
transform xs =
  [ (el, the interval)
    | HourTimeline
      {hourTimelineStartHour = startH,
       hourTimelineStartMinute = startM,
       hourTimelineEndHour = endH,
       hourTimelineEndMinute = endM,
       hourTimelineTextualIdent = textIdent,
       hourTimelineStatus = status,
       hourTimelineIdent = ident,
       hourTimelineTm = tm } <- xs,
       let start = GapItemTime startH startM,
       let end = GapItemTime endH endM,
       let el = GapItemUnit ident tm textIdent $ mkStatus status,
       let interval = (start, end),
       then group by interval using groupWith
  ] <&> \(xs, (start, end)) -> GapItem start end xs

mkLowerBound :: UTCTime -> (Year, DayOfYear) -> UTCTime
mkLowerBound tm (year, day) | snd (toOrdinalDate (utctDay tm)) == day = tm
                            | otherwise = UTCTime (fromOrdinalDate year day) 0

mkStatus :: Status -> GapItemUnitStatus
mkStatus status
  | status == ForwardedToPaymentProvider ||
    status == ProcessedByPaymentProvider = Pending
  | status == Confirmed = ProcessedOk
  | status == Declined = ProcessedDecline
  | otherwise = error $ "inappropriate status " <> show status