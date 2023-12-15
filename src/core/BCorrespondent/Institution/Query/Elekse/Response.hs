{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module BCorrespondent.Institution.Query.Elekse.Response (Response (..), toEither) where

import Data.Aeson (FromJSON (..), (.:), withObject)
import BuildInfo (location)

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