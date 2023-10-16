{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Frontend.User.ShiftTimelineHistory (handle) where

import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Model.Frontend 
       (GapItem, GapItemTime, TimelineDirection)
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM)

handle 
  :: Auth.AuthenticatedUser 'Auth.Reader  
  -> Int
  -> Int
  -> Int
  -> TimelineDirection
  -> GapItemTime
  -> KatipHandlerM (Response [GapItem])
handle _ _ _ _ _ _ = undefined