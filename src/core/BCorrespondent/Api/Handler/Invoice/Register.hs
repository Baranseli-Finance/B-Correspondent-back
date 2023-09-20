{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module BCorrespondent.Api.Handler.Invoice.Register (handle) where

import qualified BCorrespondent.Statement.Invoice as Invoice (register)
import BCorrespondent.Transport.Response (Response (Error, Ok))
import BCorrespondent.Transport.Error (asError)
import BCorrespondent.Transport.Model.Invoice (InvoiceRegisterRequest, InvoiceId)
import BCorrespondent.Statement.Auth (AccountType (User))
import qualified BCorrespondent.Auth as Auth
import Katip.Handler
import qualified Data.Text as T
import Control.Lens ((^.))
import Database.Transaction (statement, transactionM)

handle :: Auth.AuthenticatedUser -> [InvoiceRegisterRequest] -> KatipHandlerM (Response [InvoiceId])
handle Auth.AuthenticatedUser {..} _ 
  | account == User = 
    return $ Error (Just 401) $ asError @T.Text "users are forbidden to query this endpoint"
handle Auth.AuthenticatedUser {..} xs = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  fmap Ok $ transactionM hasql $ statement Invoice.register (ident, xs)