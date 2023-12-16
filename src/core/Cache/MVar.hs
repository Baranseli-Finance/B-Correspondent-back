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
  let insert key val isPermanent = 
        MVar.modifyMVar @m var $ \old -> do 
          tm <- liftIO getCurrentTime
          let tm' = if isPermanent then Nothing else Just tm
              new = Map.insert key (tm', val) old
          pure (new, True)
  let get key = fmap (fmap snd . Map.lookup key) $ MVar.readMVar var
  let update key val = do
        tm <- liftIO getCurrentTime
        MVar.modifyMVar_ @m var (return . Map.adjust (\(tmm, _) -> (fmap (const tm) tmm, val)) key)
  let delete key = MVar.modifyMVar_ @m var (return . Map.delete key)
  let clean = 
        MVar.modifyMVar_ @m var $ \x -> do 
          tm <- liftIO getCurrentTime
          flip Map.traverseMaybeWithKey x $ 
            \_ (tmm', v) ->
              case tmm' of 
                Nothing -> pure $ Just (Nothing, v) 
                Just tm' ->
                  if addUTCTime 3600 tm' > tm 
                  then pure Nothing 
                  else pure $ Just (Just tm', v)
  return Cache {..}