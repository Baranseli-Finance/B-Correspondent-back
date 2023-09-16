module BCorrespondent.Api.Handler.Transaction.GetConfirmed (handle) where

import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Model.Transaction (TransactionConfirmed)
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM)

handle :: Auth.AuthenticatedUser -> KatipHandlerM (Response [TransactionConfirmed])
handle _ = undefined