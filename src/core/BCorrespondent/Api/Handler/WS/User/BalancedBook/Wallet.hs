{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module BCorrespondent.Api.Handler.WS.User.BalancedBook.Wallet (handle) where

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


data WalletBalancedBook =
     WalletBalancedBook 
     { walletBalancedBookIdent :: Int64,
       walletBalancedBookInstitution :: Int64,
       walletBalancedBookAmount :: Double
     }
    deriving stock (Generic, Show)
    deriving
     (FromJSON, ToJSON)
     via WithOptions 
         '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined 
              (StripConstructor 
               WalletBalancedBook)]]
     WalletBalancedBook

data WalletBalancedBooks = WalletBalancedBooks { wallets :: [WalletBalancedBook] }
    deriving stock (Generic, Show)
    deriving
     (FromJSON, ToJSON)
     via WithOptions DefaultOptions
     WalletBalancedBooks

type WalletBalancedBookExt = WithField "users" [Int64] WalletBalancedBooks

type instance ListenPsql "balanced_book_wallet_update" WalletBalancedBookExt = ()

handle :: AuthenticatedUser 'Reader -> WS.Connection -> KatipHandlerM ()
handle AuthenticatedUser {institution = Nothing} conn = 
  liftIO $ sendError conn "you haven't an institution assigned to to"
handle AuthenticatedUser {ident, institution = Just inst} conn =
  withWS @Resource conn $ \db resource -> do
     $(logTM) DebugS $ fromString $ $location <> " received " <> show resource
     let mkResp (WithField userXs x)
          | ident `elem` userXs = Just $ wallets x
          | otherwise = Nothing
     withResource @"BalancedBookWallet" conn resource $ 
       listenPsql @"balanced_book_wallet_update" @WalletBalancedBookExt conn db ident mkResp