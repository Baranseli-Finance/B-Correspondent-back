{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module BCorrespondent.Api.Handler.Frontend.User.MarkNotificationRead (handle) where

import BCorrespondent.Statement.Institution (readNotification)
import BCorrespondent.Transport.Id (Id (..))
import BCorrespondent.Transport.Response (Response (Ok))
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, ask)
import Control.Lens ((^.))
import Data.Coerce (coerce)
import Database.Transaction (statement, transactionM)

handle :: Auth.AuthenticatedUser 'Auth.None -> [Id "NotificationIdent"] -> KatipHandlerM (Response ()) 
handle _ notifications = do 
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  let params = coerce notifications
  fmap (const (Ok ())) $ transactionM hasql $ statement readNotification params
