{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BCorrespondent.Transport.Model.Github (Sha (..)) where

import Data.Text (Text, stripPrefix)
import Data.Aeson
import Data.Maybe (fromMaybe)

data Sha = 
     Sha 
     { shaValue :: Text, 
       shaRepo :: Text 
     }
     deriving Show

instance FromJSON Sha where
  parseJSON = 
    withObject "Github:Sha" $ \o -> do
      shaValue <- o .: "after"
      shaRepoRaw <- 
        o .: "repository" >>=
         (.: "full_name")
      let shaRepo = 
            fromMaybe undefined $ 
              stripPrefix 
              "Baranseli-Finance/"
              shaRepoRaw   
      pure Sha {..}