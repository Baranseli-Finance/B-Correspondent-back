{-# LANGUAGE RankNTypes #-}

module Cache (Cache (..)) where

import Control.Monad.IO.Class (MonadIO)

data Cache m k v = 
     Cache
     { insert :: MonadIO m => k -> v -> m (),
       get ::  MonadIO m => k -> m (Maybe v),
       update :: MonadIO m => k -> v -> m (),
       delete :: MonadIO m => k -> m ()
     }