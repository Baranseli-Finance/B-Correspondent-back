{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module BCorrespondent.Job.Webhook.Institution.Tochka (make) where

import BCorrespondent.ServerM (ServerM)
import BCorrespondent.Job.Webhook.Factory (Webhook (..))
import Data.Aeson.Types (Value)
import Data.Text (Text)
import Data.Aeson 
       (ToJSON (toJSON), 
        object, 
        (.=), 
        FromJSON (parseJSON), 
        (.:?), 
        withObject, 
        eitherDecode,
        encode
       )
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)
import BuildInfo (location)
import Control.Applicative ((<|>))
import Data.Traversable (for)
import Data.Maybe (fromMaybe)
import Data.String.Conv (toS)
import Request (makePostReq)
import Control.Monad (join)
import Data.Bifunctor (second)
import Network.HTTP.Client 
      (Manager, 
       responseStatus, 
       HttpException (HttpExceptionRequest), 
       HttpExceptionContent (StatusCodeException)
      )
import Network.HTTP.Types (hContentType, hAuthorization)
import Network.HTTP.Types.Status (status401)
import Katip (KatipContextT, logTM, Severity (DebugS), ls)


data Method = Login | Callback
     deriving stock (Generic, Show)
     deriving (ToJSON)
      via WithOptions
          '[ConstructorTagModifier 
            '[UserDefined ToLower]]
          Method

--   "jsonrpc": "2.0",
--   "method": "login",
--   "id": "refer",
--   "params": arbitrary json object
data Request a = 
     Request 
     { requestJsonrpc :: !Text,
       requestMethod :: !Method,
       requestId :: !Text,
       requestParams :: !a
     }
    
instance ToJSON a => Show (Request a) where
  show = toS . encode

 
-- |
--
-- >>> :set -XTypeApplications
-- >>> :set -XOverloadedStrings
-- >>> import Data.Aeson (encode)
--
-- >>> let req = Request "2.0" Callback "refer" "{\"status\":\"accepted\",\"accepted_at\":\"2023-11-18T08:55:19.8836435Z\",\"external_id\":\"9185dc07-772f-452e-9fc2-b77f671133d3\"}"
-- >>> encode req
-- "{\"id\":\"refer\",\"jsonrpc\":\"2.0\",\"method\":\"callback\",\"params\":\"{\\\"status\\\":\\\"accepted\\\",\\\"accepted_at\\\":\\\"2023-11-18T08:55:19.8836435Z\\\",\\\"external_id\\\":\\\"9185dc07-772f-452e-9fc2-b77f671133d3\\\"}\"}"
instance ToJSON a => ToJSON (Request a) where
  toJSON Request {..} = 
    object 
    [ "jsonrpc" .= toJSON requestJsonrpc, 
      "method" .= toJSON requestMethod, 
      "id" .= toJSON requestId, 
      "params" .= toJSON requestParams
    ]

defRequest :: a -> Request a
defRequest = Request "2.0" Login "refer"

data Auth = Auth { email :: Text, password :: Text }
     deriving stock (Generic, Show)
     deriving (ToJSON)
      via WithOptions DefaultOptions Auth

data Error = Error { errorCode :: Int, errorMessage :: Text, errorData :: Text }
     deriving stock (Generic, Show)
     deriving (FromJSON)
      via WithOptions
          '[FieldLabelModifier 
            '[UserDefined ToLower,
              UserDefined (StripConstructor Error)]]
          Error

data Response a = Ok a | Failure Error deriving Show

toEither :: Response a -> Either String a
toEither (Ok x) = Right x
toEither (Failure (Error {..})) = Left $ toS errorMessage <> ", " <> toS errorData

data Token = Token { accessToken :: Text }
     deriving stock (Generic, Show)
     deriving (FromJSON)
      via WithOptions
          '[FieldLabelModifier 
            '[CamelTo2 "_"]]
          Token

-- | 
--
-- >>> :set -XTypeApplications
-- >>> :set -XOverloadedStrings
-- >>>  import Data.Aeson (eitherDecode)
-- >>> :{
--  let msg = "{\"jsonrpc\": \"2.0\", \"result\": { \"access_token\": \"1234\" }, \"id\": \"refer\"}"
--  in eitherDecode @(Response Token) msg  
-- :}
-- Right (Ok (Token {accessToken = "1234"}))
--
-- >>> :{
--  let msg = "{\"jsonrpc\": \"2.0\", \"error\": { \"code\": -32602, \"message\": \"Invalid login/password\", \"data\": \"error\" }, \"id\": \"refer\"}"
--  in eitherDecode @(Response ()) msg  
-- :}
-- Right (Failure (Error {errorCode = -32602, errorMessage = "Invalid login/password", errorData = "error"}))
instance FromJSON a => FromJSON (Response a) where
    parseJSON = 
      withObject ($location <> ":Response") $ \o -> do
        r <- o .:? "result"
        e <- o .:? "error"
        resp <- for r $ fmap Ok . parseJSON @a
        err <- for e $ fmap Failure . parseJSON @Error
        let msg = toS $ $location <> " couldn't parse Response, raw: " <> show o
        pure $ fromMaybe (Failure (Error 0 msg mempty)) $ resp <|> err

make :: Webhook
make =  
  Webhook 
  { send = \manager login pass msg -> 
              go manager login pass $ 
                (defRequest msg) 
                { requestMethod = Callback }
  }

data Success = Success { success :: Bool }
     deriving stock (Generic, Show)
     deriving (FromJSON)
      via WithOptions DefaultOptions Success


go :: Manager -> Text -> Text -> Request Value -> KatipContextT ServerM (Either String ())
go manager login pass req = do
  $(logTM) DebugS $ ls @Text $ $location <> " webhook request ---->  " <> toS (show req)
  tokene <- fetchAuthToken manager login pass
  fmap join $ 
    for tokene $ \(Token token) -> do
      let hs = [(hContentType, "application/json"), (hAuthorization, toS token)]
      let mkResp = second (const ()) . join . fmap toEither . eitherDecode @(Response Success) . toS
      let onFailure error =
           case error of 
              HttpExceptionRequest  _ (StatusCodeException resp _) -> 
                if status401 == responseStatus resp
                then fmap (second (const mempty)) $ go manager login pass req
                else pure . Left . toS . show $ error
              _ -> pure . Left . toS . show $ error
      fmap (join . second mkResp) $ 
        makePostReq @(Request Value) "https://letspay.to/api/v1/jrpc/b-correspondent" manager hs req onFailure


-- request:
-- curl --location --request POST 'letspay.to/api/v1/jrpc/auth' \
-- --header 'Content-Type: application/json' \
-- --data-raw '{
--     "jsonrpc": "2.0",
--     "method": "login",
--     "id": "refer",
--     "params": {
--         "email":"fclaw007@gmail.com",
--         "password":"<REDACTED>"
--     }
-- }'
-- response:
-- {
--     "jsonrpc": "2.0",
--     "result": {
--         "access_token": "....."
--     },
--     "id": "refer"
-- }
-- {
--     "jsonrpc": "2.0",
--     "error": {
--         "code": -32602,
--         "message": "Invalid login/password"
--     },
--     "id": "refer"
-- }
fetchAuthToken :: Manager -> Text -> Text -> KatipContextT ServerM (Either String Token)
fetchAuthToken manager login pass = do
  let onFailure = pure . Left . toS . show
  let req = defRequest $ Auth login pass
  let mkResp = join . fmap toEither . eitherDecode @(Response Token) . toS
  let hs = [(hContentType, "application/json")]
  fmap (join . second mkResp) $ 
    makePostReq @(Request Auth) "https://letspay.to/api/v1/jrpc/auth" manager hs req onFailure