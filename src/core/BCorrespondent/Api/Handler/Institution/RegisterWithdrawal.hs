{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}

module BCorrespondent.Api.Handler.Institution.RegisterWithdrawal (handle, mkWithdrawKey) where

import BCorrespondent.EnvKeys (Sendgrid (..))
import BCorrespondent.Statement.Institution (getWithdrawalCode)
import BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution)
import qualified BCorrespondent.Statement.Cache as Cache (insert)
import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Statement.Mail as Mail
import BCorrespondent.Transport.Error (asError)
import BCorrespondent.Transport.Response (Response (Error, Ok))
import BCorrespondent.Transport.Model.Institution (Withdraw (..))
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, sendGrid, ask)
import Database.Transaction (transactionM, statement)
import Control.Lens ((^.))
import Data.Text (Text)
import GHC.Float (floatToDigits)
import Data.Int (Int32, Int64)
import Data.String.Conv (toS)
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
import Data.Foldable (for_)
import qualified Data.Pool as Pool
import qualified Hasql.Connection as Hasql


mkWithdrawKey :: Int32 -> Text
mkWithdrawKey code = "withdrawal" <> toS (show code)

handle :: Auth.AuthenticatedUser 'Auth.Writer -> Withdraw -> KatipHandlerM (Response ())
handle user (Withdraw _ amount) 
  | sum (fst (floatToDigits 10 amount)) == 0 
    = pure $ Error Nothing $ asError @Text "cannot process 0.00"
handle user withdraw = 
  checkInstitution user $ \(user_id, _) -> do
    hasql <- fmap (^. katipEnv . hasqlDbPool) ask
    x <- generateCode hasql user_id withdraw
    fmap (const (Ok ())) $ void $ fork $ uncurry sendCode x

generateCode :: Pool.Pool Hasql.Connection -> Int64 -> Withdraw -> KatipHandlerM (Text, Int32)
generateCode hasql ident withdraw = do 
  x <- transactionM hasql $ statement getWithdrawalCode ident
  let key = mkWithdrawKey $ snd x
  isOk <- transactionM hasql $ statement Cache.insert (key, withdraw, Nothing)
  if isOk then pure x
  else generateCode hasql ident withdraw

sendCode :: Text -> Int32 -> KatipHandlerM ()
sendCode email code = do
  cfg <- fmap (^. katipEnv . sendGrid) ask
  for_ cfg $ \(Sendgrid {..}, sendgrid) -> do
    tm <- fmap (fromIntegral . systemSeconds) $ liftIO $ getSystemTime
    hasql <- fmap (^. katipEnv . hasqlDbPool) ask
    let msg = "auth code: " <> toS (show code)
    let mail = Mail.Mail email "withdrawal code" msg Mail.SendGrid
    uuid <- transactionM hasql $ statement Mail.insert mail
    let args = singleton "ident" $ toJSON uuid
    let reqBody =
          mkPOSTMailSendRequestBody 
          [mkPOSTMailSendRequestBodyContentsendgrid "text/plain" ("withdrawal code: " <> toS (show code))]
          ((mkFromEmailObject (coerce sendgridIdentity)) { fromEmailObjectName = Just "admin"})
          [((mkPOSTMailSendRequestBodyPersonalizationssendgrid [mkToEmailArrayItem email])
            {pOSTMailSendRequestBodyPersonalizationssendgridCustomArgs = Just args})
            { pOSTMailSendRequestBodyPersonalizationssendgridSendAt = Just tm,
              pOSTMailSendRequestBodyPersonalizationssendgridSubject = Just "withdrawal code"
            } ]
            "withdrawal code"
    resp <- liftIO $ void $ runWithConfiguration sendgrid (pOSTMailSend (Just reqBody))
    $(logTM) InfoS $ fromString $ $location <> " sendgrid resp ---> " <> show resp