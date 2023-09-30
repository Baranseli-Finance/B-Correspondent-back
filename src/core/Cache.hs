{-# LANGUAGE RankNTypes #-}

module Cache (Cache (..)) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value)
import Data.Text (Text)

data Cache m = 
     Cache
     { insert :: MonadIO m => Text -> Value -> m (),
       get ::  MonadIO m => Text -> m (Maybe Value),
       update :: MonadIO m => Text -> Value -> m ()
     }