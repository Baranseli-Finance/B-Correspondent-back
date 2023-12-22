module BCorrespondent.Institution.Webhook (module F, webhooks) where

import BCorrespondent.Institution.Webhook.Factory as F
import qualified BCorrespondent.Institution.Webhook.Detail.Tochka as T
import Data.Int (Int64)

webhooks :: [(Int64, Webhook)]
webhooks = [(1, T.webhook)]