{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module BCorrespondent.Api.Handler.Frontend.User.MarkNotificationRead (handle) where

import BCorrespondent.Transport.Id (Id)
import BCorrespondent.Transport.Response (Response (Ok))
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM)

handle :: Auth.AuthenticatedUser 'Auth.Reader -> Id "NotificationIdent" -> KatipHandlerM (Response ()) 
handle _ _ = return $ Ok ()
