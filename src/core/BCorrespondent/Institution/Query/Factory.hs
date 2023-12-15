{-# LANGUAGE RankNTypes #-}

module BCorrespondent.Institution.Query.Factory (Query (..)) where

import BCorrespondent.ServerM (ServerM)
import Network.HTTP.Client (Manager)
import Data.Text (Text)
import Katip (KatipContextT)
import Data.Aeson (FromJSON, ToJSON)


data Query =
     Query 
     { fetchToken :: Manager -> Text  -> Text  -> KatipContextT ServerM (Either String Text),
       makeRequest 
       :: forall body resp path 
       . (ToJSON body, Show resp, FromJSON resp) 
       => Manager 
       -> Text 
       -> Text 
       -> body 
       -> KatipContextT ServerM (Either String resp)
     }