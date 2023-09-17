{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-missing-exported-signatures #-}

module BCorrespondent.Api.Handler.Auth.Password.New (handle) where

import qualified BCorrespondent.Statement.Auth as Auth
import BCorrespondent.Transport.Model.Auth (NewPassword (..))
import BCorrespondent.Transport.Response (Response (Ok))
import Katip.Handler
import Control.Lens ((^.))
import Database.Transaction (statement, transactionM)

handle :: NewPassword -> KatipHandlerM (Response Bool)
handle NewPassword {..} = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  fmap Ok $ transactionM hasql $ statement Auth.insertNewPassword (newPasswordPassword, newPasswordKey)