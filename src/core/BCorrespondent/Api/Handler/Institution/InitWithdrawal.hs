{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Institution.InitWithdrawal (handle) where

import BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution)
import BCorrespondent.Statement.Institution (getBalances)
import BCorrespondent.Api.Handler.Utils (withError)
import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Model.Institution (InitWithdrawal (..))
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, ask)
import Database.Transaction (transactionM, statement)
import Control.Lens ((^.))

handle :: Auth.AuthenticatedUser 'Auth.Reader -> KatipHandlerM (Response InitWithdrawal)
handle user =
  checkInstitution user $
    \(_, institution_id) -> do 
      hasql <- fmap (^. katipEnv . hasqlDbPool) ask
      fmap (flip withError (`InitWithdrawal` [])) $ 
        transactionM hasql $ 
          statement getBalances institution_id