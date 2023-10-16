{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module BCorrespondent.Api.Handler.Frontend.User.ShiftTimelineHistory (handle) where

import BCorrespondent.Transport.Response (Response (Error))
import BCorrespondent.Api.Handler.Frontend.User.InitDashboard (transform)
import BCorrespondent.Statement.History (getHourShift)
import BCorrespondent.Api.Handler.Utils (withError)
import BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution)
import BCorrespondent.Transport.Error (asError)
import BCorrespondent.Transport.Model.Frontend (GapItem, TimelineDirection (..))
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM, ask, katipEnv, hasqlDbPool)
import Data.Text (Text)
import BuildInfo (location)
import Data.String (fromString)
import Katip (logTM, Severity (DebugS))
import Database.Transaction (transactionM, statement)
import Control.Lens ((^.))
import Data.Tuple.Extended (mapPolyT, consT)

handle 
  :: Auth.AuthenticatedUser 'Auth.Reader  
  -> Int
  -> Int
  -> Int
  -> TimelineDirection
  -> Int
  -> KatipHandlerM (Response [GapItem])
handle user y m d direction hour
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
      checkInstitution user $ \(_, inst_ident) -> do 
        hasql <- fmap (^. katipEnv . hasqlDbPool) ask
        let from | direction == Forward = hour
                 | otherwise = hour - 1
        let to | direction == Forward = hour + 1
               | otherwise = hour
        let params = consT inst_ident $ mapPolyT fromIntegral (y, m, d, from, to)
        $(logTM) DebugS $ fromString $ $location <> " params ---> " <> show params
        dbResp <- transactionM hasql $ statement getHourShift params
        pure $ withError dbResp transform