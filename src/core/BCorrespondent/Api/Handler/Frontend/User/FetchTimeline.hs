{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Frontend.User.FetchTimeline (handle) where

import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Transport.Model.Frontend 
       (TimelineDirection, GapItem, GapItemTime)
import BCorrespondent.Transport.Response (Response)
import Katip.Handler (KatipHandlerM)

handle :: Auth.AuthenticatedUser 'Auth.Reader -> TimelineDirection -> GapItemTime -> KatipHandlerM (Response [GapItem])
handle _ _ _ = undefined