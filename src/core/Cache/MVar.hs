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
import Control.Monad.IO.Class (MonadIO, liftIO)
import Cache
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Control
import Data.Time.Clock (getCurrentTime, addUTCTime)


init :: forall m k v . (Ord k, MonadIO m, MonadBaseControl IO m) => IO (Cache m k v)
init = do
  var <- newMVar Map.empty
  let insert key val = 
        MVar.modifyMVar @m var $ \old -> do 
          tm <- liftIO getCurrentTime
          let already = Map.member key old
              new = Map.insert key (tm, val) old
          if already
          then pure (old, False)
          else pure (new, True)
  let get key = fmap (fmap snd . Map.lookup key) $ MVar.readMVar var
  let update key val = do
        tm <- liftIO getCurrentTime
        MVar.modifyMVar_ @m var (return . Map.adjust (const (tm, val)) key)
  let delete key = MVar.modifyMVar_ @m var (return . Map.delete key)
  let clean = 
        MVar.modifyMVar_ @m var $ \x -> do 
          tm <- liftIO getCurrentTime
          flip Map.traverseMaybeWithKey x $ 
            \_ (tm', v) -> 
              if addUTCTime 3600 tm' > tm 
              then pure Nothing 
              else pure $ Just (tm', v)
  return Cache {..}