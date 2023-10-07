{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TransformListComp #-}

module BCorrespondent.Api.Handler.Frontend.Dashboard.InitDailyBalanceSheet (handle, mkStatus) where

import BCorrespondent.Transport.Model.Frontend 
       (DailyBalanceSheet (..), 
        GapItem (..), 
        GapItemTime (..), 
        GapItemUnit (..),
        GapItemUnitStatus (..)
       )
import BCorrespondent.Statement.Frontend (get1HourTimeline, HourTimeline (..))
import BCorrespondent.Transport.Response (Response (Error))
import BCorrespondent.Statement.Invoice  (Status (..))
import BCorrespondent.Transport.Error (asError)
import BCorrespondent.Api.Handler.Utils (withError)
import BCorrespondent.Auth (AuthenticatedUser (..))
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, ask)
import Control.Monad.Time (currentTime)
import Data.Time.Clock (UTCTime (..), addUTCTime)
import Data.Time.LocalTime (todMin, timeToTimeOfDay)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate, DayOfYear, Year, fromOrdinalDate)
import Database.Transaction (transactionM, statement)
import Control.Lens ((^.))
import Data.Text (Text)
import GHC.Exts (groupWith, the)
import Data.Functor ((<&>))

handle :: Auth.AuthenticatedUser 'Auth.Reader -> KatipHandlerM (Response DailyBalanceSheet)
handle AuthenticatedUser {ident, institution = Nothing} = pure $ Error (Just 403) $ asError @Text msg
  where msg = "there is no institution assigned to you. write to mailto:admin@b-correspondent.app to give the access"
handle AuthenticatedUser {institution = Just ident} = do
  tm <- currentTime
  let day = toOrdinalDate $ utctDay tm
  let shift = (mod (60 - todMin (timeToTimeOfDay (utctDayTime tm))) 5) * 60
  let upperBound = addUTCTime (fromRational (fromIntegral shift)) tm
  let lowerBoundTmp = addUTCTime (-3600) upperBound
  let lowerBound = mkLowerBound lowerBoundTmp day
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  dbResp <- transactionM hasql $ statement get1HourTimeline (lowerBound, upperBound, ident)
  pure $ withError dbResp $ \xs -> DailyBalanceSheet $ transform xs 

transform :: [HourTimeline] -> [GapItem]
transform xs =
  [ (el, the interval)
    | HourTimeline
      {hourTimelineStartHour = startH,
       hourTimelineStartMinute = startM,
       hourTimelineEndHour = endH,
       hourTimelineEndMinute = endM,
       hourTimelineTextualIdent = ident,
       hourTimelineStatus = status } <- xs,
       let start = GapItemTime startH startM,
       let end = GapItemTime endH endM,
       let el = GapItemUnit ident $ mkStatus status,
       let interval = (start, end),
       then group by interval using groupWith
  ] <&> \(xs, (start, end)) -> GapItem start end xs

mkLowerBound :: UTCTime -> (Year, DayOfYear) -> UTCTime
mkLowerBound tm (day, year) | snd (toOrdinalDate (utctDay tm)) == fromIntegral day = tm
                            | otherwise = UTCTime (fromOrdinalDate (fromIntegral year) (fromIntegral day)) 0

mkStatus :: Status -> GapItemUnitStatus
mkStatus status
  | status == ForwardedToPaymentProvider ||
    status == ProcessedByPaymentProvider = Pending
  | status == Confirmed = ProcessedOk
  | status == Declined = ProcessedDecline
  | otherwise = error $ "inappropriate status " <> show status