{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module BCorrespondent.Api.Handler.Institution.RegisterWithdrawal (handle) where

import BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution)
import BCorrespondent.Statement.Institution (registerWithdrawal, WithdrawResult (..))
import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Api.Handler.Utils (withError)
import BCorrespondent.Transport.Error (asError)
import BCorrespondent.Transport.Response (Response (Error))
import BCorrespondent.Transport.Model.Institution 
       (Withdraw (..), WithdrawResult (..))
import qualified BCorrespondent.Transport.Model.Institution as I
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, ask)
import Database.Transaction (transactionM, statement)
import Control.Lens ((^.))
import Data.Text (Text)
import GHC.Float (floatToDigits)

handle :: Auth.AuthenticatedUser 'Auth.Writer -> Withdraw -> KatipHandlerM (Response I.WithdrawResult)
handle user (Withdraw _ amount) | sum (fst (floatToDigits 10 amount)) == 0 = pure $ Error Nothing $ asError @Text "cannot process 0.00"
handle user (Withdraw ident amount) = 
  checkInstitution user $ \(user_id, _) -> do
    hasql <- fmap (^. katipEnv . hasqlDbPool) ask
    let mkResp NotEnoughFunds = WithdrawResult I.NotEnoughFunds Nothing
        mkResp (FrozenFunds amount) = WithdrawResult I.FrozenFunds $ Just amount
        mkResp Ok = WithdrawResult I.WithdrawalRegistered Nothing
    fmap (`withError` mkResp) $ transactionM hasql $ statement registerWithdrawal (user_id, ident, amount)