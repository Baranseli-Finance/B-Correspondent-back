{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Institution.GetWithdrawalHistoryPage (handle) where

import BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution)
import BCorrespondent.Statement.Institution (getWithdrawalPage)
import BCorrespondent.Api.Handler.Utils (withError)
import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Transport.Response (Response (Ok))
import BCorrespondent.Transport.Model.Institution (WithdrawalHistory)
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, ask)
import Database.Transaction (transactionM, statement)
import Control.Lens ((^.))

handle :: Auth.AuthenticatedUser 'Auth.Reader -> Int -> KatipHandlerM (Response (Maybe WithdrawalHistory))
handle _ page | page <= 0 = return $ Ok Nothing
handle user page =
  checkInstitution user $
    \(_, institution_id) -> do
      hasql <- fmap (^. katipEnv . hasqlDbPool) ask
      fmap (flip withError id) $ 
        transactionM hasql $ statement getWithdrawalPage (institution_id, 10, fromIntegral page)