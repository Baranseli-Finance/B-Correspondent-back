{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

module BCorrespondent.Api.Handler.Webhook.PaymentProvider.Elekse (handle) where

import BCorrespondent.Transport.Model.Transaction 
       (TransactionFromPaymentProvider (..))
import BCorrespondent.Statement.Transaction (create)
import BCorrespondent.Statement.Fs (InsertFile (..), insertFiles)
import BCorrespondent.Transport.Response (Response (Ok))
import Katip.Handler (KatipHandlerM, hasqlDbPool, katipEnv, ask, Minio (..), minio)
import Database.Transaction (transactionM, statement)
import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.Text (Text, pack)
import Data.Int (Int64)
import BuildInfo (location)
import Katip (logTM, logStr, Severity(ErrorS))
import Control.Monad (when)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Base64 as Base64
import Data.Traversable (for)
import Data.Either.Combinators (whenLeft)
import qualified Data.ByteString as B
import System.Directory (getTemporaryDirectory)
import Hash (mkHash)
import Data.String.Conv (toS)
import Control.Monad.IO.Class (liftIO)
import System.FilePath ((</>))
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
import Data.Coerce (coerce)      
import Control.Monad.Time (currentTime)

handle :: TransactionFromPaymentProvider -> KatipHandlerM (Response ())
handle transaction@TransactionFromPaymentProvider 
       {transactionFromPaymentProviderSwiftMessage = swift,
        transactionFromPaymentProviderIdent = uuid,
        transactionFromPaymentProviderSender = sender,
        transactionFromPaymentProviderAmount = amount,
        transactionFromPaymentProviderCurrency = currency} = do
  let fileName = sender <> "-" <>  toS (show amount) <> "-" <> toS (show currency)          
  resp <- commitSwiftMessage fileName swift
  fmap (const (Ok ())) $
    for_ resp $ \case
      [ident] -> do
        hasql <- fmap (^. katipEnv . hasqlDbPool) ask
        isOk <- transactionM hasql $ statement create (ident, transaction)
        let error = pack $ "webhook ended up in failure, ident: " <> show uuid
        when (not isOk) $ $(logTM) ErrorS $ logStr @Text $ $location <> "error ---> " <> error
      _ -> $(logTM) ErrorS $ logStr @Text $ $location <> "error ---> commitSwiftMessage returns more then 1 result"   

commitSwiftMessage :: Text -> Text -> KatipHandlerM (Either String [Int64])
commitSwiftMessage fileName s = do
  resp <- fmap join $ for (Base64.decode (encodeUtf8 s)) $ \bs -> do
    Minio {..} <- fmap (^. katipEnv . minio) ask
    tmp <- liftIO getTemporaryDirectory
    tm <- currentTime
    let hash = mkHash $ bs <> toS (show tm) 
    let path = tmp </> toS hash
    liftIO $ B.writeFile path bs
    let bucket = "transaction"
    let fileMime = "application/octet-stream"
    let file =
          InsertFile
          { insertFileHash = hash, 
            insertFileName = fileName,
            insertFileMime = fileMime,
            insertFileBucket = bucket,
            insertFileExts = []
          }
    let err = "file server failed to respond"
    minioRes <- liftIO $  
      fmap (join . maybeToRight err) $
        timeout (5 * 1_000_000) $
          fmap (first (toS . show)) $
            runMinioWith minioConn $ do
              exist <- bucketExists bucket
              unless exist $ makeBucket bucket Nothing
              let opt = 
                       defaultPutObjectOptions 
                       { pooContentType = Just fileMime }
              void $ fPutObject bucket hash path opt
    hasql <- fmap (^. katipEnv . hasqlDbPool) ask          
    for minioRes $ \_ -> fmap coerce $ transactionM hasql $ statement insertFiles [file]
  fmap (const resp) $ whenLeft resp $ \error -> $(logTM) ErrorS $ logStr @Text $ $location <> "error ---> " <> pack error