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
        InitDashboard (..),
        GapItemAmount (..)
       )
import qualified BCorrespondent.Transport.Model.Frontend as M (GapItemUnitStatus (..)) 
import BCorrespondent.Statement.Dashboard (getDashboard, TimelineGapsItem (..), Dashboard (..))
import BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution)
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Statement.Invoice  (Status (..))
import BCorrespondent.Api.Handler.Utils (withError, roundTo)
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
import Data.Bifunctor (second)
import Data.Foldable (fold)
import Data.Monoid (Sum (..))
import Data.List (nub)


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
          withError dbResp $ \Dashboard {..} ->
            InitDashboard 
            { initDashboardDailyBalanceSheet = 
                DailyBalanceSheet 
                dashboardInstitution $ 
                transform dashboardTimeline,
              initDashboardWallets = dashboardWallets  
            }
    fmap mkResp $ transactionM hasql $ statement getDashboard (lowerBound, upperBound, ident)

transform :: [TimelineGapsItem] -> [GapItem]
transform xs =
  [ (el, the interval)
    | TimelineGapsItem
      {timelineGapsItemStartHour = startH,
       timelineGapsItemStartMinute = startM,
       timelineGapsItemEndHour = endH,
       timelineGapsItemEndMinute = endM,
       timelineGapsItemTextualIdent = textIdent,
       timelineGapsItemStatus = status,
       timelineGapsItemIdent = ident,
       timelineGapsItemTm = tm,
       timelineGapsItemCurrency = currency,
       timelineGapsItemAmount = amount } <- xs,
       let start = GapItemTime startH startM,
       let end = GapItemTime endH endM,
       let el = (ident, tm, textIdent, mkStatus status, currency, amount),
       let interval = (start, end),
       then group by interval using groupWith
  ] <&> \(xs, (start, end)) ->
           let mkGapItemUnits xs = 
                 [   GapItemUnit ident tm textIdent status 
                   | (ident, tm, textIdent, status, _, _) <- nub xs
                 ]
               mkAmounts xs = 
                 [   (the currency, val) 
                   | (_, _, _, _, currency, amount) <- nub xs, 
                     let val = Sum amount, 
                     then group by currency using groupWith 
                 ] 
           in GapItem start end (mkGapItemUnits xs) (map (uncurry GapItemAmount . second (roundTo 2 . getSum . fold)) $ mkAmounts xs)

mkLowerBound :: UTCTime -> (Year, DayOfYear) -> UTCTime
mkLowerBound tm (year, day) | snd (toOrdinalDate (utctDay tm)) == day = tm
                            | otherwise = UTCTime (fromOrdinalDate year day) 0

mkStatus :: Status -> M.GapItemUnitStatus
mkStatus status
  | status == ForwardedToPaymentProvider ||
    status == ProcessedByPaymentProvider = M.Pending
  | status == Confirmed = M.Ok
  | status == Declined = M.Declined
  | otherwise = error $ "inappropriate status " <> show status