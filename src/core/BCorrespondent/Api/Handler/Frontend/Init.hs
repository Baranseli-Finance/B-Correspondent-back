module BCorrespondent.Api.Handler.Frontend.Init (handle) where

import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Model.Frontend (Init)
import BCorrespondent.Transport.Model.Auth (AuthToken)
import Katip.Handler (KatipHandlerM)

handle :: Maybe AuthToken -> KatipHandlerM (Response Init)
handle _ = undefined