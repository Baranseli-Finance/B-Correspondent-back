{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module BCorrespondent.Api.Handler.WS.User.BalancedBook.Transaction (handle) where

import BCorrespondent.Transport.Model.Invoice (Currency)
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

data TransactionBalancedBook =
     TransactionBalancedBook 
     { from :: Int, 
       to :: Int,
       amount :: Double,
       currency :: Currency,
       dow :: Int
     }
    deriving stock (Generic, Show)
    deriving
     (FromJSON, ToJSON)
     via WithOptions DefaultOptions
     TransactionBalancedBook

type TransactionBalancedBookExt = WithField "user" Int64 (WithField "institution" Int64 TransactionBalancedBook)

type instance ListenPsql "balanced_book_transaction_add" TransactionBalancedBookExt = ()

handle :: AuthenticatedUser 'Reader -> WS.Connection -> KatipHandlerM ()
handle AuthenticatedUser {institution = Nothing} conn = 
  liftIO $ sendError conn "you haven't an institution assigned to to"
handle AuthenticatedUser {ident, institution = Just inst} conn =
  withWS @Resource conn $ \db resource -> do
     $(logTM) DebugS $ fromString $ $location <> " received " <> show resource
     let mkResp (WithField dbUser (WithField dbInst x))
          | dbUser == ident &&
            dbInst == inst = Just x
          | otherwise = Nothing
     withResource @"BalancedBookTransaction" conn resource $ 
       listenPsql @"balanced_book_transaction_add" @TransactionBalancedBookExt conn db ident mkResp