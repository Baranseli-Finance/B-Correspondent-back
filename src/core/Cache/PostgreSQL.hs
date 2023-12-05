{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Cache.PostgreSQL (init) where

import Prelude hiding (init)

import qualified BCorrespondent.Statement.Cache as C
import Cache (Cache (..))
import Katip (KatipContext)
import Data.Pool (Pool)
import Hasql.Connection (Connection)
import Database.Transaction (transactionM, statement, ParamsShow)
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
import Data.Either (fromRight)


init :: forall m v . (KatipContext m, ParamsShow v, ToJSON v, FromJSON v) => Pool Connection -> IO (Cache m Text v)
init hasql = do
  let insert key val isPermanent = 
        transactionM hasql $ 
          statement C.insert (key, val, Just isPermanent)
  let get key = 
        fmap (fromRight Nothing . fmap Just) $ 
          transactionM hasql $ 
            statement C.get key
  let update key val = transactionM hasql $ statement C.update (key, val)
  let delete = transactionM hasql . statement C.delete
  let clean = pure ()
  return Cache {..}