module BCorrespondent.Api.Handler.Frontend.MakeProcuratory (handle) where

import BCorrespondent.Transport.Model.Frontend (ProcuratoryRequest)
import BCorrespondent.Transport.Response (Response)
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM)

handle :: Auth.AuthenticatedUser -> ProcuratoryRequest -> KatipHandlerM (Response ())
handle _ _ = undefined