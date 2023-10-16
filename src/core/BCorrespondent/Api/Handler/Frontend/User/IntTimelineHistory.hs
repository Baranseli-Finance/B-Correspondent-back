{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Frontend.User.IntTimelineHistory (handle) where

import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Model.Frontend (HistoryDate, HistoryTimeline (HistoryTimeline))
import BCorrespondent.Statement.History (initTimeline)
import BCorrespondent.Api.Handler.Frontend.User.InitDashboard (transform)
import BCorrespondent.Api.Handler.Utils (withError)
import qualified BCorrespondent.Auth as Auth
import Database.Transaction (transactionM, statement)
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, ask)
import Control.Lens ((^.))

handle ::  Auth.AuthenticatedUser 'Auth.Reader  -> HistoryDate -> KatipHandlerM (Response HistoryTimeline)
handle _ date = do 
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask           
  fmap (flip withError (HistoryTimeline . transform)) $ transactionM hasql $ statement initTimeline date