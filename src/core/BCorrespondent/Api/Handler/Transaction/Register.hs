{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module BCorrespondent.Api.Handler.Transaction.Register (handle) where

import BCorrespondent.Statement.Transaction ()
import BCorrespondent.Transport.Response (Response (Error))
import BCorrespondent.Transport.Error (asError)
import BCorrespondent.Transport.Model.Transaction (TransactionRegisterRequest, TransactionId)
import BCorrespondent.Statement.Auth (AccountType (User))
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM)
import qualified Data.Text as T

handle :: Auth.AuthenticatedUser -> [TransactionRegisterRequest] -> KatipHandlerM (Response [TransactionId])
handle Auth.AuthenticatedUser {..} _ | account == User = return $ Error (Just 401) $ asError @T.Text "users are forbidden to query this endpoint"
handle _ _ = undefined