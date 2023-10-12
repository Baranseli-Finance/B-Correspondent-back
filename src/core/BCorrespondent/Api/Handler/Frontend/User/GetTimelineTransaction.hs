{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Frontend.User.GetTimelineTransaction (handle) where

import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Id (Id)
import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Transport.Model.Frontend (TimelineTransaction)
import BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution)
import Katip.Handler (KatipHandlerM)

handle :: Auth.AuthenticatedUser 'Auth.Reader -> Id "transaction" -> KatipHandlerM (Response TimelineTransaction)
handle user _ = checkInstitution user $ \_ -> undefined