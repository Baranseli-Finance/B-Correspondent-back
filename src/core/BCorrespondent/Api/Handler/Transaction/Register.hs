module BCorrespondent.Api.Handler.Transaction.Register (handle) where

import BCorrespondent.Statement.Transaction ()
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Model.Transaction (TransactionRegisterRequest, TransactionId)
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM)

handle :: Auth.AuthenticatedUser -> [TransactionRegisterRequest] -> KatipHandlerM (Response [TransactionId])
handle _ _ = undefined