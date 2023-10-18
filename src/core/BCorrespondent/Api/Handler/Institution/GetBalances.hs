{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Institution.GetBalances (handle) where

import BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution)
import BCorrespondent.Transport.Model.Invoice (Currency (..))
import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Transport.Response (Response (Ok))
import BCorrespondent.Transport.Model.Institution (Balances (..), Balance (..))
import Katip.Handler (KatipHandlerM)

handle :: Auth.AuthenticatedUser 'Auth.Reader -> KatipHandlerM (Response Balances)
handle user = checkInstitution user $ \_ -> return $ Ok $ Balances [Balance USD 123.78, Balance EUR 45654.12]
