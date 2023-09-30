{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Cache.MVar (init) where

import Prelude hiding (init)
import Control.Concurrent.MVar (newMVar)
import qualified Control.Concurrent.MVar.Lifted as MVar
import Control.Monad.IO.Class (MonadIO)
import Cache
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Control

init :: forall m . (MonadIO m, MonadBaseControl IO m) => IO (Cache m)
init = do
  var <- newMVar Map.empty
  let insert key val = MVar.modifyMVar_ @m var (return . Map.insert key val)
  let get key = fmap (Map.lookup key) $ MVar.readMVar var
  let update key val = MVar.modifyMVar_ @m var (return . Map.adjust (const val) key)
  return Cache {..}