{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module BCorrespondent.Api.Handler.Fs.Upload (handle) where

import BCorrespondent.Api.Handler.Utils (withError)
import BCorrespondent.Statement.Fs (insertFiles)
import qualified BCorrespondent.Statement.Fs as Fs
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Auth (AuthenticatedUser (..), Role (None))
import BCorrespondent.Transport.Model.Fs (Bucket (..))
import BCorrespondent.Transport.Id
import Katip.Handler
import Servant.Multipart.File
import Database.Transaction
import Control.Lens
import Data.Traversable (for)
import Data.Coerce (coerce)
import Data.Text (Text)
import Control.Monad.Time (currentTime)
import Hash (mkHash)
import Data.String.Conv (toS)
import Control.Monad.IO.Class (liftIO)
import System.Directory (getTemporaryDirectory, copyFile)
import System.FilePath ((</>))
import Katip (logTM, Severity( DebugS ), logStr)
import Data.Either.Combinators (maybeToRight)
import Control.Monad (join, void, unless)
import Data.Bifunctor (first)
import System.Timeout (timeout)
import Network.Minio 
      ( runMinioWith, 
        pooContentType, 
        defaultPutObjectOptions, 
        makeBucket, 
        bucketExists, 
        fPutObject
      )

handle :: AuthenticatedUser 'None -> Bucket -> Files -> KatipHandlerM (Response [Id "file"])
handle AuthenticatedUser {..} bucket files = do
  Minio {..} <- fmap (^. katipEnv . minio) ask
  minioResp <- 
    fmap sequence $ 
      for (coerce files) $ 
        \file@File {..} -> do 
          tm <- currentTime
          let hash = 
                mkHash toS $ 
                  fileName <> 
                  fileMime <> 
                  toS (show tm)
          tmp <- liftIO getTemporaryDirectory
          let path = tmp </> toS (mkHash show file)
          liftIO $ copyFile filePath path
          let newBucket = 
                minioBucketPrefix <>
                ".user" <> 
                toS (show ident) <>
                "." <>
                coerce bucket
          $(logTM) 
            DebugS $ 
              logStr $ 
                " ----> new bucket: " <> 
                coerce @_ @Text newBucket
          let err = "file server failed to respond"
          let file =
                 Fs.File
                 { fileHash = hash, 
                   fileName = fileName, 
                   fileMime = fileMime,
                   fileBucket = newBucket,
                   fileExts = fileExts
                 }
          liftIO $
            fmap (fmap (const file) . join . maybeToRight err) $
              timeout (5 * 1_000_000) $ 
                fmap (first (toS . show)) $
                  runMinioWith minioConn $ do
                    exist <- bucketExists newBucket
                    unless exist $ makeBucket newBucket Nothing
                    let opt = 
                          defaultPutObjectOptions 
                          { pooContentType = Just fileMime }
                    void $ fPutObject newBucket hash filePath opt
  dbResp <- for minioResp $ \xs -> do 
    hasql <- fmap (^. katipEnv . hasqlDbPool) ask
    fmap coerce $ transactionM hasql $ statement insertFiles xs
  return $ withError @Text dbResp id