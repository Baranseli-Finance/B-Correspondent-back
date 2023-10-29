{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Frontend.User.InitWorkspace (handle) where

import BCorrespondent.Statement.Institution (loadUnreadNotification)
import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Transport.Model.Frontend (Workspace (..))
import BCorrespondent.Transport.Response (Response (Ok))
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, ask)
import Control.Lens ((^.))
import Database.Transaction (transactionM, statement)

handle :: Auth.AuthenticatedUser 'Auth.Reader -> KatipHandlerM (Response Workspace)
handle user = do 
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  fmap (Ok . Workspace) $ transactionM hasql $ statement loadUnreadNotification $ Auth.ident user