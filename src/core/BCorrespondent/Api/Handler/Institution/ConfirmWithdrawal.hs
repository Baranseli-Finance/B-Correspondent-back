{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module BCorrespondent.Api.Handler.Institution.ConfirmWithdrawal (handle) where

import BCorrespondent.Api.Handler.Institution.RegisterWithdrawal (mkWithdrawKey)
import BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution)
import qualified BCorrespondent.Statement.Cache as Cache (get)
import BCorrespondent.Statement.Institution (registerWithdrawal, WithdrawResult (..))
import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Api.Handler.Utils (withError)
import BCorrespondent.Transport.Error (asError)
import BCorrespondent.Transport.Response (Response (Error))
import BCorrespondent.Transport.Model.Institution 
       (Withdraw (..), WithdrawResult (..), WithdrawalCode (..))
import qualified BCorrespondent.Transport.Model.Institution as I
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, ask)
import Database.Transaction (transactionM, statement)
import Control.Lens ((^.))
import Data.Traversable (for)
import Data.String.Conv (toS)
import Data.Text (Text)


handle :: Auth.AuthenticatedUser 'Auth.Writer -> WithdrawalCode -> KatipHandlerM (Response I.WithdrawResult)
handle user (WithdrawalCode code) =
  checkInstitution user $ \(user_id, _) -> do
    hasql <- fmap (^. katipEnv . hasqlDbPool) ask
    let key = mkWithdrawKey code
    keyRes <- transactionM hasql $ statement Cache.get key
    let mkResp (Right resp) = resp
        mkResp (Left error) = Error Nothing $ asError @Text $ toS error  
    fmap mkResp $ for keyRes $ \(Withdraw ident amount) -> do
      hasql <- fmap (^. katipEnv . hasqlDbPool) ask
      let mkResp NotEnoughFunds = WithdrawResult I.NotEnoughFunds Nothing
          mkResp (FrozenFunds amount) = WithdrawResult I.FrozenFunds $ Just amount
          mkResp Ok = WithdrawResult I.WithdrawalRegistered Nothing
      fmap (`withError` mkResp) $ transactionM hasql $ statement registerWithdrawal (user_id, ident, amount)