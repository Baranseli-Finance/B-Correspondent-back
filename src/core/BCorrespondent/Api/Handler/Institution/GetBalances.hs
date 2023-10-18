{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Institution.GetBalances (handle) where

import BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution)
import BCorrespondent.Statement.Institution (getBalances)
import BCorrespondent.Api.Handler.Utils (withError)
import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Model.Institution (Balances (..))
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, ask)
import Database.Transaction (transactionM, statement)
import Control.Lens ((^.))

handle :: Auth.AuthenticatedUser 'Auth.Reader -> KatipHandlerM (Response Balances)
handle user =
  checkInstitution user $ 
    \(_, institution_id) -> do 
      hasql <- fmap (^. katipEnv . hasqlDbPool) ask
      fmap (flip withError Balances) $ 
        transactionM hasql $ 
          statement getBalances institution_id