module BCorrespondent.Api.Handler.Transaction.GetHistory (handle) where

import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Model.Transaction (Transaction)
import Katip.Handler (KatipHandlerM)

handle :: KatipHandlerM (Response [Transaction])
handle = undefined