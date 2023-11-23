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


module BCorrespondent.Job.Invoice.Provider.Elekse (make, tokenKey) where

import BCorrespondent.Statement.Institution.Auth (Institution (Elekse), insertToken)
import BCorrespondent.ServerM (ServerM, ServerState (..))
import BCorrespondent.Transport.Model.Invoice (InvoiceToPaymentProvider)
import BCorrespondent.Job.Invoice.Query (Query (..))
import qualified BCorrespondent.Job.Invoice.Query as Q
import Data.Text (Text)
import Network.HTTP.Client 
      (Manager, 
       responseStatus, 
       HttpException (HttpExceptionRequest), 
       HttpExceptionContent (StatusCodeException)
      )
import Data.Aeson (FromJSON (..), (.:), withObject, ToJSON, eitherDecode, toJSON, encode)
import BuildInfo (location)
import GHC.Generics (Generic)
import Data.Aeson.Generic.DerivingVia
import Data.String.Conv (toS)
import Request (makePostReq)
import Control.Monad (join, when)
import Data.Traversable (for)
import Data.Bifunctor (second)
import Network.HTTP.Types (hContentType, hAuthorization)
import Network.HTTP.Types.Status (unauthorized401, forbidden403)
import Katip (KatipContextT, logTM, Severity (DebugS), ls)
import Cache (Cache (..))
import qualified Control.Monad.State.Class as ST
import Control.Monad.Trans.Class (lift)
import Data.Foldable (for_)
import Database.Transaction (transactionM, statement, ask)
import Control.Lens ((^.))
import Katip.Handler (hasqlDbPool)


data Response a = Response { errorCode :: Int, body :: a }
    
instance FromJSON a => FromJSON (Response a) where
  parseJSON = 
    withObject ($location <> ":Response") $ \o -> do
      errorCode <- o .: "ErrorCode"
      raw <- o .: "Body"
      fmap (Response errorCode) $ parseJSON @a raw

toEither :: Show a => Response a -> Either String a
toEither Response {..} | errorCode == 0 = Right body
                       | otherwise = Left $ show body

tokenKey :: Text
tokenKey = "elekse_token"

invoiceUrl :: Text
invoiceUrl = "https://apigwtest.elekse.com/api/hoppa/corporate/correspondent/invoice"

authUrl :: Text
authUrl = "https://apigwtest.elekse.com/api/hoppa/auth/login/corporate"

make :: Query
make = Query { getAuthToken = fetchAuthToken, query = _query }

_query :: Manager -> Text -> InvoiceToPaymentProvider -> KatipContextT ServerM (Either String Q.Response)
_query manager token req = do
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
  let mkResp = join . fmap toEither . eitherDecode @(Response Q.Response) . toS 
  fmap (join . second mkResp) $ makePostReq @InvoiceToPaymentProvider invoiceUrl manager hs req onFailure


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

fetchAuthToken :: Manager -> Text -> Text -> KatipContextT ServerM (Either String Text)
fetchAuthToken manager login password = do
  ServerState {cache} <- lift $ ST.get
  tokenRes <- fmap (fmap (eitherDecode @Text . encode)) $ (get cache) tokenKey
  case tokenRes of 
    Just v -> fmap (const v) $ $(logTM) DebugS $ ls @Text $ $location <> " elekse token ---> " <> toS (show v)
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
        isOk <- (insert cache) tokenKey $ toJSON token
        when isOk $ do
          hasql <- fmap (^. hasqlDbPool) ask
          transactionM hasql $ statement insertToken (Elekse, token)
