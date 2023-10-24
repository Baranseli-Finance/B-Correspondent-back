{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module BCorrespondent.Api.Handler.Frontend.User.InitBalancedBook (handle) where

import qualified BCorrespondent.Transport.Model.Frontend as F
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution)
import BCorrespondent.Statement.BalancedBook (initBalancedBook, DayOfWeeksHourly (..), DayOfWeek (..), TotalOfWeekHourly (..))
import BCorrespondent.Api.Handler.Utils (withError)
import BCorrespondent.Statement.Types (DoY (..))
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, ask)
import Control.Monad.Time (currentTime)
import Data.Time.Clock (UTCTime (utctDay))
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Data.Time.Calendar (weekFirstDay, weekLastDay, DayOfWeek (Monday))
import Database.Transaction (transactionM, statement)
import Control.Lens ((^.), (<&>))
import Data.Bifunctor (second)
import Katip (logTM, Severity (DebugS))
import Data.String (fromString)
import BuildInfo (location)

handle :: Auth.AuthenticatedUser 'Auth.Reader -> KatipHandlerM (Response F.BalancedBook) 
handle user = 
  checkInstitution user $ \(_, ident) -> do
    day <- fmap utctDay currentTime
    let getDay f =
          DoY . 
          fromIntegral . 
          snd . 
          toOrdinalDate . 
          f Monday
    let startDoy =  getDay weekFirstDay day
    let endDoy = getDay weekLastDay day
    $(logTM) DebugS $
      fromString $ 
        $location <> " init balanced book from " <> 
        show (weekFirstDay Monday day) <> " to " <> 
        show (weekLastDay Monday day)
    let from = fromString $ show $ weekFirstDay Monday day
    let to = fromString $ show $ weekLastDay Monday day
    hasql <- fmap (^. katipEnv . hasqlDbPool) ask
    dbResp <- transactionM hasql $ statement initBalancedBook (startDoy, endDoy, ident)
    pure $ withError dbResp (F.BalancedBook from to . (:[]) . uncurry F.BalancedBookInstitution . second (map transform))

transform :: DayOfWeeksHourly -> F.DayOfWeeksHourly
transform DayOfWeeksHourly {..} =
  F.DayOfWeeksHourly
  { dayOfWeeksHourlyFrom = F.GapItemTime start 0,
    dayOfWeeksHourlyTo = F.GapItemTime end 0,
    dayOfWeeksHourlyAmountInDayOfWeek = 
      days <&> \DayOfWeek {..} -> 
        F.AmountInDayOfWeek dayOfWeek dayTotal,
    dayOfWeeksHourlyTotal =
      total <&> \TotalOfWeekHourly {..} -> 
        F.DayOfWeeksHourlyTotalSum currency currencyTotal
  }