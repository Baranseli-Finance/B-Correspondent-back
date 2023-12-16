{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module BCorrespondent.Statement.Delivery (TableRef (..), addAttempt) where

import Data.Aeson (FromJSON, ToJSON, toJSON)
import GHC.Generics (Generic)
import Data.Aeson.Generic.DerivingVia 
       (WithOptions (WithOptions), ConstructorTagModifier, CamelTo2)
import TH.Mk (mkArbitrary)
import qualified Hasql.Statement as HS
import Hasql.TH (resultlessStatement)
import Control.Lens (lmap)
import Data.Tuple.Extended (app1)
import Data.Int (Int64)
import Data.Text (Text)


data TableRef =
       Invoice
     | AbortedTransaction
     | Withdrawal
    deriving stock (Generic, Show)
    deriving (FromJSON, ToJSON)
      via WithOptions
          '[ConstructorTagModifier 
            '[CamelTo2 "_"]]
      TableRef

mkArbitrary ''TableRef

addAttempt :: HS.Statement (TableRef, Int64, Text) ()
addAttempt =
  lmap (app1 toJSON) 
  [resultlessStatement|
    insert into delivery
    (table_ref, table_ident, attempts, last_attempt_sent_at, error)
    values (($1 :: jsonb) #>> '{}' , $2 :: int8, 1, now(), $3 :: text)
    on conflict (table_ref, table_ident) 
    do update set 
    attempts = delivery.attempts + 1,
    error = excluded.error|]