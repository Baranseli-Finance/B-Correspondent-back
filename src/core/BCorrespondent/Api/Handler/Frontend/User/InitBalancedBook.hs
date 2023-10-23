{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module BCorrespondent.Api.Handler.Frontend.User.InitBalancedBook (handle) where

import BCorrespondent.Transport.Model.Frontend (BalancedBook (..))
import BCorrespondent.Transport.Response (Response (Ok))
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM)

handle :: Auth.AuthenticatedUser 'Auth.Reader -> KatipHandlerM (Response BalancedBook) 
handle _ = return $ Ok $ BalancedBook mempty []