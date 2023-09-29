{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module BCorrespondent.ServerM (ServerM (..), ServerException (..), ServerState (..)) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.RWS.Strict as RWS
import Control.Monad.Trans.Control
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Control.Monad.Time
import Katip.Handler
import Data.Default.Class

newtype ServerState = ServerState Int
  deriving newtype Num

instance Default ServerState where
  def = ServerState 0

newtype ServerM a = ServerM {runServerM :: RWS.RWST KatipEnv KatipLogger ServerState IO a}
  deriving newtype (Functor)
  deriving newtype (Applicative)
  deriving newtype (Monad)
  deriving newtype (MonadIO)
  deriving newtype (MonadReader KatipEnv)
  deriving newtype (MonadState ServerState)
  deriving newtype (MonadWriter KatipLogger)
  deriving newtype (MonadRWS KatipEnv KatipLogger ServerState)
  deriving newtype (MonadBase IO)
  deriving newtype (MonadBaseControl IO)
  deriving newtype (MonadCatch)
  deriving newtype (MonadThrow)
  deriving newtype (MonadUnliftIO)
  deriving newtype (MonadTime)

instance MonadUnliftIO (RWS.RWST KatipEnv KatipLogger ServerState IO) where
  withRunInIO inner = 
    RWS.RWST $ \r s -> do
      x <- withRunInIO $ \run -> 
        inner $ \m ->
          fmap fst $ RWS.evalRWST m r s
      pure (x, s, mempty)

data ServerException = RecoveryFailed
    deriving Show

instance Exception ServerException