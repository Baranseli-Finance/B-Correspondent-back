{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module BCorrespondent.Api.Handler.Admin.CreateUser (handle) where

import BCorrespondent.EnvKeys (Sendgrid (..))
import BCorrespondent.Transport.Model.Admin (NewUser (..))
import BCorrespondent.Statement.Mail as Mail 
import BCorrespondent.Statement.Admin (insertUser)
import BCorrespondent.Transport.Response (Response (Ok))
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, sendGrid, ask)
import Katip (logTM, Severity (InfoS))
import Control.Lens ((^.))
import BuildInfo (location)
import Data.String (fromString)
import Database.Transaction (transactionM, statement)
import Data.Foldable (for_)
import OpenAPI.Operations.POSTMailSend
  ( mkPOSTMailSendRequestBody,
    mkPOSTMailSendRequestBodyContentsendgrid,
    mkPOSTMailSendRequestBodyPersonalizationssendgrid,
    pOSTMailSend,
    pOSTMailSendRequestBodyPersonalizationssendgridSendAt,
    pOSTMailSendRequestBodyPersonalizationssendgridSubject,
    pOSTMailSendRequestBodyPersonalizationssendgridCustomArgs
  )
import OpenAPI.Types.FromEmailObject (mkFromEmailObject, fromEmailObjectName)
import "sendgrid" OpenAPI.Common
import Data.Coerce (coerce)
import OpenAPI.Types.ToEmailArray (mkToEmailArrayItem)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Lifted (fork)
import Data.Aeson (toJSON)
import Data.Aeson.KeyMap (singleton)


handle :: NewUser -> KatipHandlerM (Response ())
handle user@NewUser {..} = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  void $ transactionM hasql $ statement insertUser user
  fmap (const (Ok ())) $ fork $ sendCredToEmail newUserEmail newUserLogin newUserPassword
  
sendCredToEmail email login pass = do
  cfg <- fmap (^. katipEnv . sendGrid) ask
  for_ cfg $ \(Sendgrid {..}, sendgrid) -> do
    tm <- fmap (fromIntegral . systemSeconds) $ liftIO $ getSystemTime
    hasql <- fmap (^. katipEnv . hasqlDbPool) ask
    let msg = 
             "The new user has been registered \
             \ on B-Correspondent under the temporary password: \
             \ login " <> login <> ", password: " <> pass <>
             ". We recommend setting a new password \
             \ once you have been logged in. link: https://b-correspondent.app"
    let mail = Mail.Mail email "new user registration" msg Mail.SendGrid
    uuid <- transactionM hasql $ statement Mail.insert mail
    let args = singleton "ident" $ toJSON uuid
    let reqBody =
          mkPOSTMailSendRequestBody 
          [mkPOSTMailSendRequestBodyContentsendgrid "text/plain" msg]
          ((mkFromEmailObject (coerce sendgridIdentity)) { fromEmailObjectName = Just "admin"})
          [((mkPOSTMailSendRequestBodyPersonalizationssendgrid [mkToEmailArrayItem email])
            {pOSTMailSendRequestBodyPersonalizationssendgridCustomArgs = Just args})
            { pOSTMailSendRequestBodyPersonalizationssendgridSendAt = Just tm,
              pOSTMailSendRequestBodyPersonalizationssendgridSubject = Just $ "new user registration"
            } ]
            "new user registration"
    resp <- liftIO $ runWithConfiguration sendgrid (pOSTMailSend (Just reqBody))
    $(logTM) InfoS $ fromString $ $location <> " sendgrid resp ---> " <> show resp