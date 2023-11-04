{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module BCorrespondent.Api.Handler.WS.User.Transaction (handle) where

import BCorrespondent.Api.Handler.Frontend.User.InitDashboard (mkStatus)
import BCorrespondent.Auth (AuthenticatedUser (..), Role (..))
import BCorrespondent.Statement.Invoice (Status)
import BCorrespondent.Api.Handler.WS.Utils (withWS, ListenPsql, listenPsql, sendError, Resource (..), withResource)
import Katip.Handler (KatipHandlerM)
import qualified Network.WebSockets.Connection as WS
import Data.Text (Text)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.WithField (WithField (..))
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Katip (Severity (DebugS), logTM)
import Data.String (fromString)
import BuildInfo (location)
import Data.Int (Int64)

data Transaction = 
     Transaction 
     { transactionDayOfYear :: Int,
       transactionHour :: Int,
       transactionMin :: Int,
       transactionTextualIdent :: Text,
       transactionTm :: Text,
       transactionIdent :: Int64
     }
    deriving stock (Generic, Show)
     deriving
     (FromJSON, ToJSON)
     via WithOptions
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower, 
              UserDefined
              (StripConstructor Transaction)]]
          Transaction

type TransactionExt = Maybe (WithField "institutionId" Int64 (WithField "status" Status Transaction))

type instance ListenPsql "dashboard_transaction" TransactionExt = ()

handle :: AuthenticatedUser 'Reader -> WS.Connection -> KatipHandlerM ()
handle AuthenticatedUser {institution = Nothing} conn = 
  liftIO $ sendError conn "you haven't an institution assigned to to"
handle AuthenticatedUser {ident, institution = Just inst} conn =
  withWS @Resource conn $ \db resource -> do
     $(logTM) DebugS $ fromString $ $location <> " received " <> show resource
     let mkResp (Just (WithField dbInst x)) 
           | dbInst == inst = Just $ first mkStatus x
           | otherwise = Nothing
         mkResp _ = Nothing    
     withResource @"Transaction" conn resource $ 
       listenPsql @"dashboard_transaction" @TransactionExt conn db ident mkResp