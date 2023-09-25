{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module BCorrespondent.Api.Handler.Auth.ResendAuthCode (handle) where

import BCorrespondent.Api.Handler.Auth.SendAuthCode (sendAuthCode)
import qualified BCorrespondent.Statement.Auth as Auth
import BCorrespondent.Transport.Model.Auth (AuthCodeHash (..), ResendCode (..))
import BCorrespondent.Transport.Response (Response (Error, Ok, Warnings))
import BCorrespondent.Transport.Error (AsError (asError))
import Control.Lens
import Database.Transaction
import qualified Data.Text as T
import Katip.Handler
import Control.Concurrent.Lifted (fork)
import Control.Monad (void)
import Data.String.Conv (toS)

data Error = Code

instance Show Error where
  show Code = "code wrong"

handle :: ResendCode -> KatipHandlerM (Response AuthCodeHash)
handle ResendCode {..} = do 
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  let mkResponse Nothing = pure $ Error Nothing $ asError @T.Text $ toS (show Code)
      mkResponse (Just (Right (Auth.NextAttemptIn value))) = 
        pure $ Warnings (AuthCodeHash mempty) [asError @T.Text $ toS (show value)]
      mkResponse (Just (Left e)) = pure $ Error Nothing $ asError @T.Text $ toS e
      mkResponse (Just (Right (Auth.HashAndCode hash code email))) = do
        void $ fork $ sendAuthCode code email
        return $ Ok (AuthCodeHash hash)
  mkResponse =<< transactionM hasql (statement Auth.insertResendCode (resendCodeHash, resendCodeBrowserFp))