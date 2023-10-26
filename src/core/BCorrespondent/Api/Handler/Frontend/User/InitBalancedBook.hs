{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module BCorrespondent.Api.Handler.Frontend.User.InitBalancedBook (handle, initDayOfWeeksHourly, transform) where

import qualified BCorrespondent.Transport.Model.Frontend as F
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution)
import BCorrespondent.Statement.BalancedBook 
       (initFirstBalancedBook, initSecondBalancedBook, 
        DayOfWeeksHourly (..), DayOfWeek (..), TotalOfWeekHourly (..))
import BCorrespondent.Api.Handler.Utils (withError)
import BCorrespondent.Statement.Types (DoY (..))
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, ask)
import Control.Monad.Time (currentTime)
import Data.Time.Clock (UTCTime (utctDay), addUTCTime)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Data.Time.Calendar (weekFirstDay, weekLastDay, DayOfWeek (..))
import Database.Transaction (transactionM, statement)
import Control.Lens ((^.), (<&>))
import Katip (logTM, Severity (DebugS))
import Data.String (fromString)
import BuildInfo (location)
import Data.List (find)
import Data.Maybe
import Data.Tuple.Extended (uncurryT, app2)

-- 1 the current day is equal to weekFirstDay: we have only one day, there is no past
-- 2 not: two parts: past (weekFirstDay, last), now: current day

handle :: Auth.AuthenticatedUser 'Auth.Reader -> KatipHandlerM (Response F.BalancedBook) 
handle user = 
  checkInstitution user $ \(_, ident) -> do
    tm <-currentTime
    let day = utctDay tm
    let startDay = weekFirstDay Monday day
    let prevDay = utctDay $ addUTCTime (-86400) tm 
    let endDay | day == startDay = Nothing
               | otherwise = Just prevDay

    let getDoy = DoY . fromIntegral . snd . toOrdinalDate

    let nowDoy = getDoy day 
    let startDoy =  getDoy startDay
    let endDoy = fmap getDoy endDay

    let msg = 
             " init balanced book from " <> 
             show (weekFirstDay Monday day) <> " to " <> 
             show (weekLastDay Monday day) <> ", past part: " <> 
             show startDay <> " - " <> show endDay <> ", now part: " <> show day

    $(logTM) DebugS $ fromString $ $location <> msg

    let from = fromString $ show $ weekFirstDay Monday day
    let to = fromString $ show $ weekLastDay Monday day
    hasql <- fmap (^. katipEnv . hasqlDbPool) ask
    let go xs =
            F.BalancedBook from to $ xs <&>
              (uncurryT F.BalancedBookInstitution . 
                app2 (transform initDayOfWeeksHourly))
    fmap (`withError` go) $ transactionM hasql $ do
      first <- statement initFirstBalancedBook (startDoy, endDoy, nowDoy, ident)
      second <- statement initSecondBalancedBook (startDoy, endDoy, nowDoy, ident)
      return $ sequence [first, second]

initDayOfWeeksHourly :: [DayOfWeeksHourly]
initDayOfWeeksHourly = 
  [ item 
    | (s, e) <- zip [0 ..23] [1 .. 24],
      let xs = map (`DayOfWeek` 0) [fromEnum Monday .. fromEnum Sunday],
      let e' = if e == 24 then 0 else e,
      let item = DayOfWeeksHourly s e' xs []
  ]

transform :: [DayOfWeeksHourly] -> [DayOfWeeksHourly] -> [F.DayOfWeeksHourly]
transform xs ys = 
  xs <&> \item@DayOfWeeksHourly {start = s, end = e, days = daysXs} ->
    let y =
          fromMaybe item $
          flip find ys $ \x -> 
            start x == start item && 
            end x == end item
    in F.DayOfWeeksHourly
       { dayOfWeeksHourlyFrom = F.GapItemTime s 0,
         dayOfWeeksHourlyTo = F.GapItemTime e 0,
         dayOfWeeksHourlyAmountInDayOfWeek = 
          daysXs <&> \item@DayOfWeek {dayOfWeek = dow} ->
            let x = 
                   fromMaybe item $ 
                     flip find (days y) $ \x -> dayOfWeek x == dow
            in F.AmountInDayOfWeek dow $ dayTotal x,
         dayOfWeeksHourlyTotal = 
          total y <&> \TotalOfWeekHourly {..} -> 
            F.DayOfWeeksHourlyTotalSum currency currencyTotal
       }