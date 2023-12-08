{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module BCorrespondent.Api.Handler.Institution.ConfirmWithdrawal (handle) where

import BCorrespondent.Api.Handler.Institution.RegisterWithdrawal (mkWithdrawKey)
import BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution)
import BCorrespondent.Notification (makeH, WithdrawalRegister (..))
import qualified BCorrespondent.Statement.Cache as Cache (get, delete)
import BCorrespondent.Statement.Institution (registerWithdrawal, WithdrawResult (..))
import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Api.Handler.Utils (withEither)
import BCorrespondent.Transport.Error (asError)
import BCorrespondent.Transport.Response (Response (Error))
import qualified BCorrespondent.Transport.Response as R
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
      let mkResp NotEnoughFunds = pure $ WithdrawResult I.NotEnoughFunds Nothing
          mkResp (FrozenFunds amount) = pure $ WithdrawResult I.FrozenFunds $ Just amount
          mkResp (Ok instId currency) =
            let notification = 
                  WithdrawalRegister 
                  { withdrawalRegisterCurrency = currency
                  , withdrawalRegisterAmount = amount 
                  }
            in fmap (const (WithdrawResult I.WithdrawalRegistered Nothing)) $
               makeH @"new_withdrawal" instId [notification]
      dbRes <- transactionM hasql $
        statement Cache.delete key >>
        statement registerWithdrawal (user_id, ident, amount)
      withEither dbRes $ fmap R.Ok . mkResp