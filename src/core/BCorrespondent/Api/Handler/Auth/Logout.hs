{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Auth.Logout (handle) where

import qualified BCorrespondent.Statement.Auth as Auth
import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Transport.Response (Response (Ok, Error))
import BCorrespondent.Transport.Error (asError)
import Katip.Handler
import Control.Lens
import Database.Transaction
import Data.Bool (bool)
import Data.Text (Text) 

handle :: Auth.AuthenticatedUser 'Auth.Reader -> KatipHandlerM (Response ())
handle Auth.AuthenticatedUser {Auth.jwtIdent} = do 
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  let msg = asError @Text "something went wrong during logout"
  fmap (bool (Error Nothing msg) (Ok ())) $ 
    transactionM hasql (statement Auth.logout jwtIdent)