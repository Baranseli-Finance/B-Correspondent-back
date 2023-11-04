{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module BCorrespondent.Api.Handler.WS.User.Notification (handle) where

import BCorrespondent.Auth (AuthenticatedUser (..), Role (..))
import BCorrespondent.Api.Handler.WS.Utils (withWS, ListenPsql, listenPsql, sendError, Resource (..), withResource)
import Katip.Handler (KatipHandlerM)
import qualified Network.WebSockets.Connection as WS
import Control.Monad.IO.Class (liftIO)
import Katip (Severity (DebugS), logTM)
import Data.String (fromString)
import BuildInfo (location)
import Data.Int (Int64)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics
import Data.Aeson (FromJSON)


data Counter = Counter { institutionId :: Int64, count :: Int64 }
  deriving (Generic, Show)
   deriving (FromJSON)
       via WithOptions DefaultOptions
          Counter

type instance ListenPsql "notification" Counter = ()

handle :: AuthenticatedUser 'Reader -> WS.Connection -> KatipHandlerM ()
handle AuthenticatedUser {institution = Nothing} conn = 
  liftIO $ sendError conn "you haven't an institution assigned to to"
handle AuthenticatedUser {ident, institution = Just inst} conn =
  withWS @Resource conn $ \db resource -> do
     $(logTM) DebugS $ fromString $ $location <> " received " <> show resource
     let mkResp (Counter {institutionId, count})
           | institutionId == inst = Just count
           | otherwise = Nothing
     withResource @"Notification" conn resource $
       listenPsql @"notification" @Counter conn db ident mkResp