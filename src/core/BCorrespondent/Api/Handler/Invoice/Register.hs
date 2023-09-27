{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Invoice.Register (handle) where

import qualified BCorrespondent.Statement.Invoice as Invoice (register)
import BCorrespondent.Transport.Response (Response (Error), fromEither)
import BCorrespondent.Transport.Error (asError)
import BCorrespondent.Transport.Model.Invoice (InvoiceRegisterRequest, InvoiceRegisterResponse)
import BCorrespondent.Statement.Auth (AccountType (User))
import qualified BCorrespondent.Auth as Auth
import Katip.Handler
import qualified Data.Text as T
import Control.Lens ((^.))
import Database.Transaction (statement, transactionM)
import Data.String.Conv (toS)
import Data.Bifunctor (first)

handle :: Auth.AuthenticatedUser 'Auth.Bank -> [InvoiceRegisterRequest] -> KatipHandlerM (Response [InvoiceRegisterResponse])
handle Auth.AuthenticatedUser {..} _ 
  | account == User = 
    return $ Error (Just 401) $ asError @T.Text "users are forbidden to query this endpoint"
handle Auth.AuthenticatedUser {..} xs = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  fmap (fromEither @T.Text . first toS) $ transactionM hasql $ statement Invoice.register (ident, xs)