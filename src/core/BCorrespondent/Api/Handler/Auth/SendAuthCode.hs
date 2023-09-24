{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}

module BCorrespondent.Api.Handler.Auth.SendAuthCode (handle) where

import BCorrespondent.EnvKeys (Sendgrid (..))
import qualified BCorrespondent.Statement.Auth as Auth
import BCorrespondent.Transport.Model.Auth (Credentials (..), AuthCodeHash (..))
import BCorrespondent.Transport.Response (Response (Error, Ok, Warnings))
import BCorrespondent.Transport.Error (AsError (asError))
import Control.Lens
import Database.Transaction
import Katip.Handler
import qualified Data.Text as T
import Data.String.Conv (toS)
import Data.Foldable (for_)
import OpenAPI.Operations.POSTMailSend
  ( mkPOSTMailSendRequestBody,
    mkPOSTMailSendRequestBodyContentsendgrid,
    mkPOSTMailSendRequestBodyPersonalizationssendgrid,
    pOSTMailSend,
    pOSTMailSendRequestBodyPersonalizationssendgridSendAt,
    pOSTMailSendRequestBodyPersonalizationssendgridSubject,
  )
import OpenAPI.Types.FromEmailObject (mkFromEmailObject, fromEmailObjectName)
import "sendgrid" OpenAPI.Common
import Data.Coerce (coerce)
import OpenAPI.Types.ToEmailArray (mkToEmailArrayItem)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (($>))

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
      mkResponse (Just (Right (Auth.HashAndCode hash code email))) = 
        sendAuthCode code email $> Ok (AuthCodeHash hash)
  mkResponse =<< transactionM hasql (statement Auth.insertCode (login, password))

sendAuthCode :: Int -> T.Text -> KatipHandlerM ()
sendAuthCode code email = do
  cfg <- fmap (^. katipEnv . sendGrid) ask
  for_ cfg $ \(Sendgrid {..}, sendgrid) -> do
    tm <- fmap (fromIntegral . systemSeconds) $ liftIO $ getSystemTime
    hasql <- fmap (^. katipEnv . hasqlDbPool) ask
    let reqBody = 
          mkPOSTMailSendRequestBody 
          [mkPOSTMailSendRequestBodyContentsendgrid "text/plain" ("auth code: " <> toS (show code))]
          ((mkFromEmailObject (coerce sendgridIdentity)) { fromEmailObjectName = Just "admin"})
          [(mkPOSTMailSendRequestBodyPersonalizationssendgrid [mkToEmailArrayItem email])
            { pOSTMailSendRequestBodyPersonalizationssendgridSendAt = Just tm,
              pOSTMailSendRequestBodyPersonalizationssendgridSubject = Just $ "auth code"
            } ]
            "auth code"
    liftIO $ void $ runWithConfiguration sendgrid (pOSTMailSend (Just reqBody))