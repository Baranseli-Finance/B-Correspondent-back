{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module BCorrespondent.Job.Backup.Google (obtainAccessToken, AccessToken (..)) where

import Network.Google.OAuth2.JWT (getSignedJWT, fromPEMString)
import Data.String.Conv (toS)
import qualified Network.HTTP.Client as HTTP
import qualified Data.Text as T
import Request (makePostReq')
import Data.Traversable (for)
import Data.Aeson (FromJSON (..), (.:), withObject, eitherDecodeStrict)
import Control.Monad (join)

data AccessToken = AccessToken T.Text
  deriving Show

instance FromJSON AccessToken where
  parseJSON = 
    withObject "AccessToken" $ \o -> 
      fmap AccessToken $ 
        o .: "access_token"

-- curl -s -X POST https://www.googleapis.com/oauth2/v4/token \
--  --data-urlencode 'grant_type=urn:ietf:params:oauth:grant-type:jwt-bearer' \
--  --data-urlencode "assertion=$jwt_token" \
--  | jq -r .access_token
obtainAccessToken :: HTTP.Manager -> T.Text -> T.Text -> T.Text -> IO (Either String AccessToken)
obtainAccessToken manager url email privateKey = do
  pk <- fromPEMString $ toS privateKey
  jwte <- getSignedJWT email (Just email) ["https://www.googleapis.com/auth/cloud-platform"] Nothing pk
  fmap join $ for jwte $ \jwt -> do
    let getKey bs = eitherDecodeStrict @AccessToken bs
    let onFailure error = pure $ Left $ show error  
    fmap (join . fmap getKey) $ makePostReq' (url <> toS (show jwt)) manager onFailure