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
{-# LANGUAGE AllowAmbiguousTypes #-}


module BCorrespondent.Institution.Query.Elekse.Request (make) where

import BCorrespondent.ServerM (ServerM, ServerState (..))
import BCorrespondent.Institution.Query.Elekse.Response (Response (..), toEither)
import BCorrespondent.Institution.Query.Elekse.Token (tokenKey)
import Data.Text (Text)
import Network.HTTP.Client 
      (Manager, 
       responseStatus, 
       HttpException (HttpExceptionRequest), 
       HttpExceptionContent (StatusCodeException)
      )
import Data.Aeson (FromJSON (..), ToJSON, eitherDecode)
import BuildInfo (location)
import Data.String.Conv (toS)
import Request (makePostReq)
import Control.Monad (join)
import Data.Bifunctor (second)
import Network.HTTP.Types (hContentType, hAuthorization)
import Network.HTTP.Types.Status (unauthorized401, forbidden403)
import Katip (KatipContextT, logTM, Severity (DebugS), ls)
import Cache (Cache (..))
import qualified Control.Monad.State.Class as ST
import Control.Monad.Trans.Class (lift)


baseUrl :: Text
baseUrl = "https://apigwtest.elekse.com/api/hoppa/corporate/correspondent/"

make :: forall body resp . (ToJSON body, FromJSON resp, Show resp) => Manager -> Text -> Text -> body -> KatipContextT ServerM (Either String resp)
make manager path token req = do
  let hs = [(hContentType, "application/json"), (hAuthorization, "Bearer " <> toS token)]
  let onFailure error = 
        case error of
          HttpExceptionRequest  _ (StatusCodeException resp _) ->
            if unauthorized401 == 
                responseStatus resp ||
                forbidden403 == responseStatus resp
            then
              fmap (second (const mempty)) $ do
                $(logTM) DebugS $ ls @Text $ 
                  $location <> 
                  " elekse ----> " <> 
                  toS (show (responseStatus resp)) <> 
                  " error, regenerate token"
                ServerState {cache} <- lift $ ST.get
                fmap (Left . const "token error") $ (delete cache) tokenKey
            else pure . Left . toS . show $ error
          _ -> pure . Left . toS . show $ error
  let mkResp = join . fmap toEither . eitherDecode @(Response resp) . toS
  let url =  baseUrl <> path
  fmap (join . second mkResp) $ makePostReq @body url manager hs req onFailure
