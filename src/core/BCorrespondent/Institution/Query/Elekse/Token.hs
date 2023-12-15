{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}


module BCorrespondent.Institution.Query.Elekse.Token (fetch, tokenKey) where

import BCorrespondent.Statement.Institution.Auth (Institution (Elekse), insertToken)
import BCorrespondent.Institution.Query.Elekse.Response (Response (..))
import BCorrespondent.ServerM (ServerM, ServerState (..))
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Data.Aeson (FromJSON (..), ToJSON, eitherDecode, toJSON, encode)
import BuildInfo (location)
import GHC.Generics (Generic)
import Data.Aeson.Generic.DerivingVia
import Data.String.Conv (toS)
import Request (makePostReq)
import Control.Monad (join, when)
import Data.Traversable (for)
import Data.Bifunctor (second)
import Network.HTTP.Types (hContentType)
import Katip (KatipContextT, logTM, Severity (DebugS), ls)
import Cache (Cache (..))
import qualified Control.Monad.State.Class as ST
import Control.Monad.Trans.Class (lift)
import Data.Foldable (for_)
import Database.Transaction (transactionM, statement, ask)
import Control.Lens ((^.))
import Katip.Handler (hasqlDbPool)


tokenKey :: Text
tokenKey = "elekse_token"

authUrl :: Text
authUrl = "https://apigwtest.elekse.com/api/hoppa/auth/login/corporate"


data Credentials = 
     Credentials 
     { credentialsUsername :: Text, 
       credentialsPassword :: Text 
     } 
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions
          '[FieldLabelModifier
            '[UserDefined
              (StripConstructor 
               Credentials)]]
      Credentials

fetch :: Manager -> Text -> Text -> KatipContextT ServerM (Either String Text)
fetch manager login password = do
  ServerState {cache} <- lift $ ST.get
  tokenRes <- fmap (fmap (eitherDecode @Text . encode)) $ (get cache) tokenKey
  case tokenRes of 
    Just v -> 
      fmap (const v) $ 
        $(logTM) DebugS $ 
          ls @Text $ 
            $location <> " elekse token ---> " <> toS (show v)
    Nothing -> do
      let hs = [(hContentType, "application/json")]
      let onFailure = pure . Left . toS . show
      let req = Credentials login password
      let mkResp bs = 
            let tmp = eitherDecode @(Response Text) . toS $ bs
            in join $ for tmp $ \(Response {..}) -> 
                        if errorCode == 0
                        then Right body
                        else Left $ "auth failed: " <> show errorCode
      resp <- fmap (join . second mkResp) $ makePostReq @Credentials authUrl manager hs req onFailure
      fmap (const resp) $ for_ resp $ \token -> do
        isOk <- (insert cache) tokenKey (toJSON token) True
        when isOk $ do
          hasql <- fmap (^. hasqlDbPool) ask
          transactionM hasql $ statement insertToken (Elekse, token)
