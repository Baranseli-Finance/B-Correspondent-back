module BCorrespondent.Api.Handler.Transaction.New (handle) where

import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Model.Transaction (TransactionNewRequest, TransactionId)
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM)

handle :: Auth.AuthenticatedUser -> [TransactionNewRequest] -> KatipHandlerM (Response [TransactionId])
handle _ _ = undefined