{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module BCorrespondent.Api.Handler.Auth.SendAuthCode (handle, sendAuthCode) where

import BCorrespondent.EnvKeys (Sendgrid (..))
import qualified BCorrespondent.Statement.Auth as Auth
import BCorrespondent.Transport.Model.Auth (Credentials (..), AuthCodeHash (..))
import BCorrespondent.Transport.Response (Response (Error, Ok, Warnings))
import BCorrespondent.Transport.Error (AsError (asError))
import BCorrespondent.Statement.Mail as Mail
import Control.Lens
import Database.Transaction
import Katip.Handler
import qualified Data.Text as T
import Data.String.Conv (toS)
import Data.Foldable (for_)
import Katip (logTM, Severity (InfoS))
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
import BuildInfo (location)
import Data.String (fromString)


data Error = User404

instance Show Error where
  show User404 = "user wrong"

handle :: Credentials -> KatipHandlerM (Response AuthCodeHash)
handle Credentials {..} = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  let mkResponse Nothing = pure $ Error Nothing $ asError @T.Text $ toS (show User404)
      mkResponse (Just (Right (Auth.NextAttemptIn value))) = 
        pure $ Warnings (AuthCodeHash mempty) [asError @T.Text $ toS (show value)]
      mkResponse (Just (Left e)) = pure $ Error Nothing $ asError @T.Text $ toS e
      mkResponse (Just (Right (Auth.HashAndCode hash code email))) = do
        void $ fork $ sendAuthCode code email
        return $ Ok (AuthCodeHash hash)
  mkResponse =<< transactionM hasql (statement Auth.insertCode (login, password, browserFp))

sendAuthCode :: Int -> T.Text -> KatipHandlerM ()
sendAuthCode code email = do
  cfg <- fmap (^. katipEnv . sendGrid) ask
  for_ cfg $ \(Sendgrid {..}, sendgrid) -> do
    tm <- fmap (fromIntegral . systemSeconds) $ liftIO $ getSystemTime
    hasql <- fmap (^. katipEnv . hasqlDbPool) ask
    let msg = "auth code: " <> toS (show code)
    let mail = Mail.Mail email "auth code" msg Mail.SendGrid
    uuid <- transactionM hasql $ statement Mail.insert mail
    let args = singleton "ident" $ toJSON uuid
    let reqBody =
          mkPOSTMailSendRequestBody 
          [mkPOSTMailSendRequestBodyContentsendgrid "text/plain" ("auth code: " <> toS (show code))]
          ((mkFromEmailObject (coerce sendgridIdentity)) { fromEmailObjectName = Just "admin"})
          [((mkPOSTMailSendRequestBodyPersonalizationssendgrid [mkToEmailArrayItem email])
            {pOSTMailSendRequestBodyPersonalizationssendgridCustomArgs = Just args})
            { pOSTMailSendRequestBodyPersonalizationssendgridSendAt = Just tm,
              pOSTMailSendRequestBodyPersonalizationssendgridSubject = Just $ "auth code"
            } ]
            "auth code"
    resp <- liftIO $ void $ runWithConfiguration sendgrid (pOSTMailSend (Just reqBody))
    $(logTM) InfoS $ fromString $ $location <> " sendgrid resp ---> " <> show resp