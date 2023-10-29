{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module BCorrespondent.Api.Handler.Frontend.User.GetNotifications (handle) where

import BCorrespondent.Statement.Institution (loadNotification)
import BCorrespondent.Transport.Model.Frontend (Notifications)
import BCorrespondent.Transport.Response (Response)
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, ask)
import BCorrespondent.Api.Handler.Utils  (withError)
import Control.Lens ((^.))
import Database.Transaction (statement, transactionM)

handle :: Auth.AuthenticatedUser 'Auth.Reader -> KatipHandlerM (Response Notifications) 
handle user = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  fmap (`withError` id) $ transactionM hasql $ statement loadNotification $ Auth.ident user

