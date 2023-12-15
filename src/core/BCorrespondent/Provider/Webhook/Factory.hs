module BCorrespondent.Provider.Webhook.Factory (Webhook (..)) where

import BCorrespondent.ServerM (ServerM)
import Data.Aeson.Types (Value)
import Network.HTTP.Client (Manager)
import Data.Text (Text)
import Katip (KatipContextT)


data Webhook = Webhook { send :: Manager -> Text -> Text -> Value -> KatipContextT ServerM (Either String ()) }