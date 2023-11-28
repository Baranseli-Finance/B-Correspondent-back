{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

-- https://martinheinz.dev/blog/105
module BCorrespondent.Statement.Cache (insert, get) where

import qualified Hasql.Statement as HS
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON, encode, toJSON, eitherDecode, Value)
import Control.Lens (rmap, dimap)
import Data.Bifunctor (second)
import Hasql.TH (rowsAffectedStatement, maybeStatement)
import Data.Maybe (fromMaybe)

insert :: ToJSON a => HS.Statement (Text, a) Bool
insert = 
  dimap (second toJSON) (>0)
  [rowsAffectedStatement|
    insert into cache
    (key, value, inserted_at) 
    values ($1 :: text, $2 :: jsonb, now()) 
    on conflict do nothing|]

get :: forall a . FromJSON a => HS.Statement Text (Either String a)
get = 
  rmap (fromMaybe (Left "key not found") . fmap (eitherDecode @a . encode @Value)) 
  [maybeStatement| select value :: jsonb from cache where key = $1 :: text|]