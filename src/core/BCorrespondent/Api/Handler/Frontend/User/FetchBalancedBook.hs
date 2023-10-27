{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module BCorrespondent.Api.Handler.Frontend.User.FetchBalancedBook (handle) where

import BCorrespondent.Api.Handler.Frontend.User.InitBalancedBook (initDayOfWeeksHourly, transform)
import qualified BCorrespondent.Api.Handler.Frontend.User.InitBalancedBook as InitBalancedBook
import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Transport.Model.Frontend (BalancedBook (..), BalancedBookDirection (..), BalancedBookInstitution (..))
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Id (Id (..))
import BCorrespondent.Statement.BalancedBook (fetchFirstBalancedBook, fetchSecondBalancedBook)
import BCorrespondent.Api.Handler.Utils (withError)
import BCorrespondent.Statement.Types (DoY (..))
import BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution)
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, ask)
import Control.Lens ((^.), (<&>))
import Data.Time.Calendar.OrdinalDate (Day, toOrdinalDate)
import Data.Time.Calendar (weekFirstDay, weekLastDay, addDays, DayOfWeek (..))
import Database.Transaction (transactionM, statement)
import Data.String (fromString)
import Control.Monad.Time (currentTime)
import Data.Time.Clock (UTCTime (utctDay))
import Data.Tuple.Extended (uncurryT, app3)

handle
  :: Auth.AuthenticatedUser 'Auth.Reader  
  -> Id "year"
  -> Id "month"
  -> Id "day"
  -> BalancedBookDirection
  -> KatipHandlerM (Response BalancedBook)
handle user (Id y) (Id m) (Id d) direction = 
  checkInstitution user $ \(_, ident) -> do 
    let addZero x = if x < 10 then "0" <> show x else show x
    let givenDay = read @Day $ show y <> "-" <> addZero m <> "-" <> addZero d
    let day | direction == BalancedBookDirectionForward = addDays 1 givenDay
            | otherwise = addDays (-1) givenDay
    let getDoy f = DoY . fromIntegral . snd . toOrdinalDate . f Monday
    let startDoy = getDoy weekFirstDay day
    let endDoy = getDoy weekLastDay day

    tm <-currentTime
    let doy = DoY $ fromIntegral $ snd $ toOrdinalDate $ utctDay tm
     
    let fetchPast = do
          hasql <- fmap (^. katipEnv . hasqlDbPool) ask
          let from = fromString $ show $ weekFirstDay Monday day
          let to = fromString $ show $ weekLastDay Monday day
          let go xs = 
                   zip [1..] xs <&> \(idx, x) ->
                     uncurryT BalancedBookInstitution $
                       app3 (transform (initDayOfWeeksHourly idx)) x
          fmap (`withError` BalancedBook from to . go) $ transactionM hasql $ do 
            first <- statement fetchFirstBalancedBook (startDoy, endDoy, ident)
            second <- statement fetchSecondBalancedBook (startDoy, endDoy, ident)
            return $ sequence [first, second]

    if doy >=startDoy && doy <= endDoy then InitBalancedBook.handle user else fetchPast