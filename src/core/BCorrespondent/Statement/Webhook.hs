{-# LANGUAGE QuasiQuotes #-}

module BCorrespondent.Statement.Webhook (fetch, markDelivered) where

import qualified Hasql.Statement as HS
import Hasql.TH (vectorStatement, resultlessStatement)
import Data.Int (Int64)
import Data.Aeson.Types (Value)
import Data.UUID (UUID)
import Control.Lens (rmap, lmap)
import Data.Vector (toList, fromList)
import Data.Text (Text) 


fetch :: HS.Statement () [(UUID, Int64, Value, Text, Text)]
fetch = 
  rmap toList 
  [vectorStatement|
    select 
      id :: uuid, 
      w.institution_id :: int8, 
      message :: jsonb,
      wc.login :: text,
      wc.password :: text 
    from webhook as w
    inner join webhook_credentials as wc
    on w.institution_id = wc.institution_id
    where not is_delivered|]

markDelivered :: HS.Statement [UUID] ()
markDelivered = lmap fromList [resultlessStatement|update webhook set is_delivered = true where id = any($1 :: uuid[])|]