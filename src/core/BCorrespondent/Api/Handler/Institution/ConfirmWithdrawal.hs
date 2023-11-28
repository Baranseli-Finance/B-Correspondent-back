{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module BCorrespondent.Api.Handler.Institution.ConfirmWithdrawal (handle) where

import BCorrespondent.Api.Handler.Institution.RegisterWithdrawal (mkWithdrawKey)
import BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution)
import BCorrespondent.Statement.Institution (registerWithdrawal, WithdrawResult (..))
import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Api.Handler.Utils (withError)
import BCorrespondent.Transport.Error (asError)
import BCorrespondent.Transport.Response (Response (Error))
import BCorrespondent.Transport.Model.Institution 
       (Withdraw (..), WithdrawResult (..), WithdrawalCode (..))
import qualified BCorrespondent.Transport.Model.Institution as I
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, cache, ask)
import Database.Transaction (transactionM, statement)
import Control.Lens ((^.))
import Data.Aeson (encode, eitherDecode)
import Cache (Cache (Cache, get, delete))
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Data.String.Conv (toS)
import Data.Bifunctor (first)


handle :: Auth.AuthenticatedUser 'Auth.Writer -> WithdrawalCode -> KatipHandlerM (Response I.WithdrawResult)
handle user (WithdrawalCode code) =
  checkInstitution user $ \(user_id, _) -> do
    Cache {get, delete} <- fmap (^. katipEnv . cache) ask
    let key = mkWithdrawKey code
    let msg = "key " <> key <> " not found"
    let parse = 
          fromMaybe (Left msg) . 
          fmap (first toS . eitherDecode @Withdraw . encode)
    keyRes <- fmap parse $ get key
    let mkResp (Right resp) = resp
        mkResp (Left error) = Error Nothing $ asError error  
    fmap mkResp $ for keyRes $ \(Withdraw ident amount) -> do
      hasql <- fmap (^. katipEnv . hasqlDbPool) ask
      let mkResp NotEnoughFunds = WithdrawResult I.NotEnoughFunds Nothing
          mkResp (FrozenFunds amount) = WithdrawResult I.FrozenFunds $ Just amount
          mkResp Ok = WithdrawResult I.WithdrawalRegistered Nothing
      resp <- fmap (`withError` mkResp) $ 
                transactionM hasql $ 
                  statement 
                    registerWithdrawal 
                    (user_id, ident, amount)
      fmap (const resp) $ delete key