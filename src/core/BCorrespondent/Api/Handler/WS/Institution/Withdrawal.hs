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

import BCorrespondent.Transport.Model.Institution (WithdrawalHistoryItem (..))
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

type Item = WithField "inst_ident" Int64 WithdrawalHistoryItem

type instance ListenPsql "withdrawal" Item = ()

handle :: AuthenticatedUser 'Reader -> WS.Connection -> KatipHandlerM ()
handle AuthenticatedUser {institution = Nothing} conn = 
  liftIO $ sendError conn "you haven't an institution assigned to to"
handle AuthenticatedUser {ident, institution = Just inst_id} conn =
  withWS @Resource conn $ \db resource -> do 
    $(logTM) DebugS $ fromString $ $location <> " received " <> show resource
    let mkResp (WithField id item)
          | id == inst_id = Just item
          | otherwise = Nothing
    withResource @"Withdrawal" conn resource $ listenPsql @"withdrawal" @Item conn db ident mkResp