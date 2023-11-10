{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module BCorrespondent.Api.Handler.Frontend.User.SubmitIssue (handle) where

import BCorrespondent.EnvKeys (Sendgrid (..), Person (..))
import BCorrespondent.Statement.Fs (fetchFiles, File (..))
import BCorrespondent.Transport.Model.Frontend (Issue (..))
import BCorrespondent.Transport.Response (Response (Ok))
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, ask, Minio (..), minio, sendGrid)
import Control.Concurrent.Lifted (fork)
import Control.Monad (void, join)
import Control.Lens ((^.), (<&>))
import Data.Traversable (for)
import Database.Transaction (transactionM, statement)
import qualified Network.Minio as Minio
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import qualified Conduit as Conduit
import Data.String.Conv (toS)
import qualified Data.ByteString.Lazy as BL
import Data.Bifunctor
import OpenAPI.Operations.POSTMailSend
  ( mkPOSTMailSendRequestBody,
    mkPOSTMailSendRequestBodyContentsendgrid,
    mkPOSTMailSendRequestBodyPersonalizationssendgrid,
    pOSTMailSend,
    pOSTMailSendRequestBodyPersonalizationssendgridSendAt,
    pOSTMailSendRequestBodyPersonalizationssendgridSubject,
    mkPOSTMailSendRequestBodyAttachmentssendgrid,
    pOSTMailSendRequestBodyAttachments
  )
import OpenAPI.Types.FromEmailObject (mkFromEmailObject, fromEmailObjectName)
import "sendgrid" OpenAPI.Common
import OpenAPI.Types.ToEmailArray (mkToEmailArrayItem, toEmailArrayItemName)
import Data.Foldable (for_)
import Data.Coerce (coerce)
import qualified Data.ByteString.Base64 as B64
import Data.Text.Encoding (decodeUtf8)


handle :: Issue -> KatipHandlerM (Response ()) 
handle issue = fmap (const Ok ()) $ void $ fork $ go issue

go Issue {issueDescription, issueFiles} = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  Minio {..} <- fmap (^. katipEnv . minio) ask
  let files = maybe [] concat issueFiles

  dbResp <- transactionM hasql $ statement fetchFiles files
  ys <- for dbResp $ \xs -> 
    for xs $ \File {..} -> do
      liftIO $ Minio.runMinioWith minioConn $ do
        o <- Minio.getObject fileBucket fileHash Minio.defaultGetObjectOptions
        tm <- (toS . show . systemSeconds) <$> liftIO getSystemTime
        let (ext:_) = fileExts
        let tmpFileName = toS fileName <> "_" <> tm <> "." <> toS ext
        path <- Conduit.runConduit $ 
          Minio.gorObjectStream o 
          Conduit..| 
          Conduit.sinkSystemTempFile tmpFileName
        let title = toS fileName <> "." <> toS ext
        fmap (title,) $ liftIO $ BL.readFile path 
  let files = join $ second (first (toS .show)) $ second sequence ys
  attachmentsRes <- 
    for files $ \xs -> 
      for xs $ \(title, bs) -> 
       pure $ 
         decodeUtf8 (B64.encode (toS bs)) 
           `mkPOSTMailSendRequestBodyAttachmentssendgrid` 
         title

  for_ attachmentsRes $ \attachments -> do
    cfg <- fmap (^. katipEnv . sendGrid) ask
    for_ cfg $ \(Sendgrid {..}, sendgrid) -> do
      tm <- fmap (fromIntegral . systemSeconds) $ liftIO $ getSystemTime
      let recipient = 
            sendgridPersons <&> \Person {..} -> 
              (mkToEmailArrayItem personEmail) 
              {toEmailArrayItemName = Just personPersonalization }
      let reqBody =
            (mkPOSTMailSendRequestBody
            [mkPOSTMailSendRequestBodyContentsendgrid "text/plain" issueDescription]
            ((mkFromEmailObject (coerce sendgridIdentity)) { fromEmailObjectName = Just "admin"})
            [(mkPOSTMailSendRequestBodyPersonalizationssendgrid recipient)
              { pOSTMailSendRequestBodyPersonalizationssendgridSendAt = Just tm,
                pOSTMailSendRequestBodyPersonalizationssendgridSubject = Just "technical issue"
              } ]
            "technical issue")
            {pOSTMailSendRequestBodyAttachments = if null attachments then Nothing else Just attachments}
      liftIO $ void $ runWithConfiguration sendgrid (pOSTMailSend (Just reqBody))