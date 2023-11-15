module BCorrespondent.Job.Webhook.Factory (Webhook (..)) where

import Data.Aeson.Types (Value)
import Network.HTTP.Client (Manager)
import Data.Text (Text)

data Webhook = Webhook { send :: Manager -> Text -> Text -> Value -> IO (Either String ()) }