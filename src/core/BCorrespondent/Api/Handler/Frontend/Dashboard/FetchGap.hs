{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Frontend.Dashboard.FetchGap (handle) where

import BCorrespondent.Transport.Model.Frontend (GapItemTime, GapItem)
import BCorrespondent.Transport.Response (Response)
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM)

handle :: Auth.AuthenticatedUser 'Auth.Reader -> GapItemTime -> GapItemTime -> KatipHandlerM (Response GapItem)
handle _ _ _ = undefined