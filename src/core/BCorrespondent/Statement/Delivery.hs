{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module BCorrespondent.Statement.Delivery (TableRef (..), addAttempt, setDelivered) where

import Data.Aeson (FromJSON, ToJSON, toJSON)
import GHC.Generics (Generic)
import Data.Aeson.Generic.DerivingVia 
       (WithOptions (WithOptions), ConstructorTagModifier, CamelTo2)
import TH.Mk (mkArbitrary)
import qualified Hasql.Statement as HS
import Hasql.TH (singletonStatement, resultlessStatement)
import Control.Lens (lmap)
import Data.Tuple.Extended (app1, app2)
import Data.Int (Int64)
import Data.Text (Text)
import Database.Transaction (ParamsShow (..))
import Data.Vector (fromList)


data TableRef =
       Invoice
     | Transaction
     | Withdrawal
    deriving stock (Generic, Show)
    deriving (FromJSON, ToJSON)
      via WithOptions
          '[ConstructorTagModifier 
            '[CamelTo2 "_"]]
      TableRef

mkArbitrary ''TableRef

instance ParamsShow TableRef where
  render = show . toJSON

addAttempt :: HS.Statement (TableRef, Int64, Text) Bool
addAttempt =
  lmap (app1 toJSON) 
  [singletonStatement|
    insert into delivery
    (table_ref, table_ident, attempts, last_attempt_sent_at, error)
    values (($1 :: jsonb) #>> '{}', $2 :: int8, 1, now(), $3 :: text)
    on conflict (table_ref, table_ident)
    do update set
    attempts = delivery.attempts + 1,
    error = excluded.error,
    is_stuck = delivery.attempts + 1 = 20
    returning is_stuck :: bool|]

setDelivered :: HS.Statement (TableRef, [Int64]) ()
setDelivered = 
  lmap (app1 toJSON . app2 fromList) 
  [resultlessStatement|
    update delivery 
    set is_delivered = true
    where table_ref = ($1 :: jsonb) #>> '{}' 
    and table_ident = any($2 :: int8[])|]