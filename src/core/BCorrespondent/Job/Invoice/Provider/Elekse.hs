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

module BCorrespondent.Job.Invoice.Provider.Elekse (make) where

import BCorrespondent.Transport.Model.Invoice (InvoiceToPaymentProvider)
import BCorrespondent.Job.Invoice.Query (Query (..))
import Data.Text (Text)
import Network.HTTP.Client 
      (Manager, 
       responseStatus, 
       HttpException (HttpExceptionRequest), 
       HttpExceptionContent (StatusCodeException)
      )
import Data.Aeson (FromJSON (..), (.:), withObject, ToJSON, eitherDecode)
import BuildInfo (location)
import GHC.Generics (Generic)
import Data.Aeson.Generic.DerivingVia
import Data.String.Conv (toS)
import Request (makePostReq)
import Control.Monad (join)
import Data.Traversable (for)
import Data.Bifunctor (second)
import Network.HTTP.Types (hContentType, hAuthorization)
import Network.HTTP.Types.Status (status401)


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

make :: Query
make = Query { query = go }

go :: Manager -> Text -> Text -> InvoiceToPaymentProvider -> IO (Either String ())
go manager login pass req = do 
  tokene <- fetchAuthToken manager login pass
  fmap join $
    for tokene $ \token -> do 
      let hs = [(hContentType, "application/json"), (hAuthorization, "Bearer " <> toS token)]
      let onFailure error = 
            case error of 
              HttpExceptionRequest  _ (StatusCodeException resp _) ->
                if status401 == responseStatus resp
                then fmap (second (const mempty)) $ go manager login pass req
                else pure . Left . toS . show $ error
              _ -> pure . Left . toS . show $ error
      let mkResp = second (const ()) . join . fmap toEither . eitherDecode @(Response ()) . toS 
      fmap (join . second mkResp) $
        makePostReq @InvoiceToPaymentProvider "https://apigwtest.elekse.com/api/hoppa/corporate/correspondent/invoice" manager hs req onFailure


data Credentials = Credentials { credentialsUsername :: Text, credentialsPassword :: Text } 
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined
              (StripConstructor 
               Credentials)]]
      Credentials

fetchAuthToken :: Manager -> Text -> Text -> IO (Either String Text)
fetchAuthToken manager login password = do
  let hs = [(hContentType, "application/json")]
  let onFailure = pure . Left . toS . show
  let req = Credentials login password
  let mkResp bs = 
        let tmp = eitherDecode @(Response Text) . toS $ bs
        in join $ for tmp $ \(Response {..}) -> 
                    if errorCode == 0
                    then Right body
                    else Left $ "auth failed: " <> show errorCode
  fmap (join . second mkResp) $ makePostReq @Credentials "https://apigwtest.elekse.com/api/hoppa/auth/login/corporate" manager hs req onFailure