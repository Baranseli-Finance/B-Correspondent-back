{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Institution.Withdraw (handle) where

import BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution)
import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Model.Institution (Withdraw)
import Katip.Handler (KatipHandlerM)

handle :: Auth.AuthenticatedUser 'Auth.Writer -> Withdraw -> KatipHandlerM (Response ())
handle user _ = 
  checkInstitution user $ \_ -> undefined

