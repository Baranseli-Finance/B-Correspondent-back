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
import Data.Aeson.WithField (WithField (..))
import Data.Int (Int64)

data Wallet = 
     Wallet
     { walletIdent :: Int,
       walletAmount :: Double
     }
    deriving stock (Generic, Show)
     deriving
     (FromJSON, ToJSON)
     via WithOptions
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower, 
              UserDefined
              (StripConstructor Wallet)]]
          Wallet


type WalletExt = WithField "user" Int64 Wallet

type instance ListenPsql "dashboard_wallet" WalletExt = ()

handle :: AuthenticatedUser 'Reader -> WS.Connection -> KatipHandlerM ()
handle AuthenticatedUser {institution = Nothing} conn = 
  liftIO $ sendError conn "you haven't an institution assigned to to"
handle AuthenticatedUser {ident, institution = Just _} conn =
  withWS @Resource conn $ \db resource -> do 
     $(logTM) DebugS $ fromString $ $location <> " received " <> show resource
     let mkResp (WithField dbUser x)
          | dbUser == ident = Just x
          | otherwise = Nothing
     withResource @"Wallet" conn resource $ listenPsql @"dashboard_wallet" @WalletExt conn db ident mkResp