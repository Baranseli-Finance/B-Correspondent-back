module BCorrespondent.Api.Handler.Frontend.User.SubmitIssue (handle) where

import BCorrespondent.Transport.Model.Frontend (Issue (..))
import BCorrespondent.Transport.Response (Response (Ok))
import Katip.Handler (KatipHandlerM)

handle :: Issue -> KatipHandlerM (Response ()) 
handle Issue {issueDescription, issueFiles} = return $ Ok ()