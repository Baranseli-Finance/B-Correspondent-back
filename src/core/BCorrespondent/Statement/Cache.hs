{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

-- https://martinheinz.dev/blog/105
module BCorrespondent.Statement.Cache (insert, get, delete, update) where

import qualified Hasql.Statement as HS
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON, encode, toJSON, eitherDecode, Value)
import Control.Lens (rmap, dimap, lmap)
import Data.Bifunctor (second)
import Hasql.TH (rowsAffectedStatement, maybeStatement, resultlessStatement)
import Data.Maybe (fromMaybe)

insert :: ToJSON a => HS.Statement (Text, a) Bool
insert = 
  dimap (second toJSON) (>0)
  [rowsAffectedStatement|
    insert into cache (key, value) 
    values ($1 :: text, $2 :: jsonb) 
    on conflict do nothing|]

get :: forall a . FromJSON a => HS.Statement Text (Either String a)
get = 
  rmap (fromMaybe (Left "key not found") . fmap (eitherDecode @a . encode @Value))
  [maybeStatement| select value :: jsonb from cache where key = $1 :: text|]

delete :: HS.Statement Text ()
delete = [resultlessStatement|delete from cache where key = $1 :: text|]

update :: ToJSON a => HS.Statement (Text, a) ()
update = lmap (second toJSON)  [resultlessStatement|update cache set value = $2 :: jsonb where key = $1 :: text|]