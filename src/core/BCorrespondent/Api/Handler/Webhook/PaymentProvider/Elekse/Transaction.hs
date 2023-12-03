{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Webhook.PaymentProvider.Elekse.Transaction (handle) where

import qualified BCorrespondent.Statement.Cache as Cache (insert)
import BCorrespondent.Transport.Model.Transaction
       (TransactionFromPaymentProvider (..))
import BCorrespondent.Statement.Transaction (create)
import BCorrespondent.Statement.Fs (File (..), insertFiles)
import BCorrespondent.Transport.Response (Response (Ok))
import Katip.Handler (KatipHandlerM, hasqlDbPool, katipEnv, ask, Minio (..), minio)
import BCorrespondent.Notification (makeH, Transaction (..)) 
import Database.Transaction (transactionM, statement)
import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.Text (Text, pack)
import Data.Int (Int64)
import BuildInfo (location)
import Katip (logTM, logStr, Severity(ErrorS))
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
import Control.Concurrent.Lifted (fork)
import Network.Mime (defaultMimeLookup, fileNameExtensions)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import System.FilePath (extSeparator)
import Data.UUID (toString, UUID)



mkTransactionKey :: UUID -> Text
mkTransactionKey uuid = toS $ "transaction" <> toString uuid

handle :: TransactionFromPaymentProvider -> KatipHandlerM (Response ())
handle transaction@TransactionFromPaymentProvider 
       {transactionFromPaymentProviderSwiftMessage = swift,
        transactionFromPaymentProviderSwiftMessageExt = ext,
        transactionFromPaymentProviderIdent = uuid,
        transactionFromPaymentProviderSender = sender,
        transactionFromPaymentProviderAmount = amount,
        transactionFromPaymentProviderCurrency = currency} = 
  fmap (const (Ok ())) $ fork $ do
    hasql <- fmap (^. katipEnv . hasqlDbPool) ask
    void $ transactionM hasql $ statement Cache.insert (mkTransactionKey uuid, transaction, Just True)
    tm <- liftIO $ fmap systemSeconds getSystemTime
    let fileName = 
          sender <> "_" <> 
          toS (show amount) <> "_" <> 
          toS (show currency) <> "_" <> 
          toS (show tm) <> 
          toS [extSeparator] <>
          ext
    resp <- commitSwiftMessage fileName swift
    for_ resp $ \[ident] -> do
      dbRes <- transactionM hasql $ statement create (ident, transaction)
      for_ dbRes $ \(instIdent, textualIdent) -> 
        makeH @"transaction_processed" instIdent [Transaction textualIdent uuid]
     

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
    let fileMime = toS $ defaultMimeLookup fileName
    let file =
          File
          { fileHash = hash, 
            fileName = fileName,
            fileMime = fileMime,
            fileBucket = bucket,
            fileExts = fileNameExtensions fileName
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