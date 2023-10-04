{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Frontend.Dashboard.MakeProcuratory (handle) where

import BCorrespondent.Transport.Model.Frontend (ProcuratoryRequest)
import BCorrespondent.Transport.Response (Response)
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM)

handle :: Auth.AuthenticatedUser 'Auth.Writer -> ProcuratoryRequest -> KatipHandlerM (Response ())
handle _ _ = undefined