{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module BCorrespondent.Api.Handler.Auth.Password.MakeResetLink (handle) where

import BCorrespondent.Api.Handler.Utils (withError)
import qualified BCorrespondent.Statement.Auth as Auth
import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Statement.Mail as Mail 
import BCorrespondent.EnvKeys (Sendgrid (..))
import Control.Lens
import Database.Transaction
import Control.Concurrent.Lifted (fork)
import Data.Foldable (for_)
import Control.Monad (void)
import Katip (logTM, Severity (InfoS))
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Hash (mkHash512)
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
import Katip.Handler
import Control.Monad.IO.Class
import Data.Int (Int64)
import Data.Aeson (toJSON)
import Data.Aeson.KeyMap (singleton)
import BuildInfo (location)
import Data.String (fromString)


handle :: Auth.AuthenticatedUser 'Auth.Reader -> KatipHandlerM (Response (Maybe Int64))
handle Auth.AuthenticatedUser {..} = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  tm <- fmap (fromIntegral . systemSeconds) $ liftIO $ getSystemTime
  let hash = mkHash512 $ show tm <> show ident
  res <- transactionM hasql $ statement Auth.insertPasswordResetLink (ident, hash)
  for_ res $ \case
    Auth.Success email -> do
      void $ fork $ do
        cfg <- fmap (^. katipEnv . sendGrid) ask
        for_ cfg $ \(Sendgrid {..}, sendgrid) -> do
            let link = "password reset link: https://b-correspondent.app/#/auth/password/reset?key=" <> hash
            let mail = Mail.Mail email "password reset" link Mail.SendGrid
            uuid <- transactionM hasql $ statement Mail.insert mail
            let args = singleton "ident" $ toJSON uuid
            let reqBody = 
                  mkPOSTMailSendRequestBody
                  [mkPOSTMailSendRequestBodyContentsendgrid "text/plain" link]
                  ((mkFromEmailObject (coerce sendgridIdentity)) { fromEmailObjectName = Just "admin"})
                  [((mkPOSTMailSendRequestBodyPersonalizationssendgrid [mkToEmailArrayItem email])
                    {pOSTMailSendRequestBodyPersonalizationssendgridCustomArgs = Just args})
                  { pOSTMailSendRequestBodyPersonalizationssendgridSendAt = Just tm,
                    pOSTMailSendRequestBodyPersonalizationssendgridSubject = Just $ "password reset"
                  } ]
                  "password reset"
            resp <- liftIO $ void $ runWithConfiguration sendgrid (pOSTMailSend (Just reqBody))
            $(logTM) InfoS $ fromString $ $location <> " sendgrid resp ---> " <> show resp
    _ -> return ()     
  return $ withError res \case (Auth.TMLeft sec) -> Just sec; _ -> Nothing
