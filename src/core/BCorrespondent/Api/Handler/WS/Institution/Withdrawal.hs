{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module BCorrespondent.Api.Handler.WS.Institution.Withdrawal (handle) where

import BCorrespondent.Transport.Model.Institution (WithdrawalHistory)
import BCorrespondent.Auth (AuthenticatedUser (..), Role (..))
import Katip.Handler (KatipHandlerM)
import qualified Network.WebSockets.Connection as WS
import BCorrespondent.Api.Handler.WS.Utils (withWS, ListenPsql, listenPsql, sendError, Resource (..), withResource)
import Control.Monad.IO.Class (liftIO)
import BuildInfo (location)
import Katip (Severity (DebugS), logTM)
import Data.String (fromString)
import Data.Aeson.WithField (WithField (..))
import Data.Int (Int64)

type WithdrawalHistoryExt = WithField "user" Int64 (WithField "institution" Int64 WithdrawalHistory)

type instance ListenPsql "withdrawal" WithdrawalHistoryExt = ()

handle :: AuthenticatedUser 'Reader -> WS.Connection -> KatipHandlerM ()
handle AuthenticatedUser {institution = Nothing} conn = 
  liftIO $ sendError conn "you haven't an institution assigned to to"
handle AuthenticatedUser {ident, institution = Just inst_id} conn =
  withWS @Resource conn $ \db resource -> do 
    $(logTM) DebugS $ fromString $ $location <> " received " <> show resource
    let mkResp (WithField user (WithField institution item))
          | institution == inst_id && 
            ident == user = 
              Just item
          | otherwise = Nothing
    withResource @"Withdrawal" conn resource $ listenPsql @"withdrawal" @WithdrawalHistoryExt conn db ident mkResp
    