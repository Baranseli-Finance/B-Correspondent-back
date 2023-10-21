{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Frontend.User.GetNotifications (handle) where

import BCorrespondent.Transport.Model.Frontend (Notifications (..))
import BCorrespondent.Transport.Response (Response (Ok))
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM)

handle :: Auth.AuthenticatedUser 'Auth.Reader  -> KatipHandlerM (Response Notifications) 
handle _ = return $ Ok $ Notifications 45 []

