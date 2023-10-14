{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module BCorrespondent.Api.Handler.WS.User.Wallet (handle) where

import BCorrespondent.Auth (AuthenticatedUser (..), Role (..))
import BCorrespondent.Api.Handler.WS.Utils (withWS, ListenPsql, listenPsql, sendError, Resource (..), withResource)
import Katip.Handler (KatipHandlerM)
import qualified Network.WebSockets.Connection as WS
import Data.Aeson.Generic.DerivingVia
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Control.Monad.IO.Class (liftIO)
import Katip (Severity (DebugS), logTM)
import Data.String (fromString)
import BuildInfo (location)

data Wallet = 
     Wallet
     { walletIdent :: Int,
       walletAmount :: Double
     }
    deriving stock (Generic)
     deriving
     (FromJSON, ToJSON)
     via WithOptions
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower, 
              UserDefined
              (StripConstructor Wallet)]]
          Wallet


type instance ListenPsql "wallet" Wallet = ()

handle :: AuthenticatedUser 'Reader -> WS.Connection -> KatipHandlerM ()
handle AuthenticatedUser {institution = Nothing} conn = 
  liftIO $ sendError conn "you haven't an institution assigned to to"
handle AuthenticatedUser {ident, institution = Just _} conn =
  withWS @Resource conn $ \db resource -> do 
     $(logTM) DebugS $ fromString $ $location <> " received " <> show resource
     withResource @"Wallet" conn resource $ listenPsql @"wallet" @Wallet conn db ident id