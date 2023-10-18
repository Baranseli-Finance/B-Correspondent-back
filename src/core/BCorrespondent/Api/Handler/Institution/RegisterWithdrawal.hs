{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Institution.RegisterWithdrawal (handle) where

import BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution)
import BCorrespondent.Statement.Institution (registerWithdrawal, WithdrawResult (..))
import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Api.Handler.Utils (withError)
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Transport.Model.Institution 
       (Withdraw (..), WithdrawResult (..))
import qualified BCorrespondent.Transport.Model.Institution as I
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, ask)
import Database.Transaction (transactionM, statement)
import Control.Lens ((^.))

handle :: Auth.AuthenticatedUser 'Auth.Writer -> Withdraw -> KatipHandlerM (Response I.WithdrawResult)
handle user (Withdraw ident amount) = 
  checkInstitution user $ \_ -> do
    hasql <- fmap (^. katipEnv . hasqlDbPool) ask
    let mkResp NotEnoughFunds = WithdrawResult I.NotEnoughFunds Nothing
        mkResp (FrozenFunds amount) = WithdrawResult I.FrozenFunds $ Just amount
        mkResp Ok = WithdrawResult I.WithdrawalRegistered Nothing
    fmap (`withError` mkResp) $ transactionM hasql $ statement registerWithdrawal (ident, amount)