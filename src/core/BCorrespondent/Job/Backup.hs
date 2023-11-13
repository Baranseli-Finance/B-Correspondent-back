{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE BangPatterns #-}

module BCorrespondent.Job.Backup (run) where


import BCorrespondent.ServerM (ServerM)
import BCorrespondent.Job.Utils (withElapsedTime)
import BCorrespondent.Statement.Backup (insert)
import BCorrespondent.Job.Backup.Google (AccessToken (..), obtainAccessToken) 
import BCorrespondent.EnvKeys (Google (..))
import Katip
import BuildInfo (location)
import Control.Monad (forever)
import Control.Concurrent.Lifted (threadDelay)
import Control.Monad.Time (currentTime)
import Data.Time.Clock (utctDay)
import Control.Monad (when, void)
import Control.Monad.Trans.State.Strict (evalStateT, get, modify')
import Control.Monad.Trans.Class (lift)
import Hash (mkHashMd5)
import System.Process (readProcess)
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple.Internal (ConnectInfo (..))
import Control.Lens ((^.))
import Katip.Handler (psqlConn, hasqlDbPool, google, httpReqManager, symmetricKeyBase, ask)
import System.Directory (getTemporaryDirectory)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.Transaction (statement, transactionM)
import Data.Text (Text)
import Data.Foldable (for_)
import Data.Either.Combinators (whenLeft)
import Data.String.Conv (toS)
import Network.HTTP.Client.MultipartFormData (partBS)
import qualified Request (safeMake, methodPost)
import Network.HTTP.Types.Header (hAuthorization, hContentType)
import Data.ByteString (ByteString)
import Crypto.Cipher.Symmetric (twofish128Key, twofish128IV, twofish128)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Base64 as B64 
import Data.Text.Encoding (decodeUtf8)


run :: Int -> KatipContextT ServerM ()
run freq = do
  tm <-currentTime
  let !day = utctDay tm
  flip evalStateT day $ do
    forever $ do  
      threadDelay $ freq * 1_000_000
      currDay <- get
      tm <-currentTime
      let !day = utctDay tm
      when (day /= currDay) $ do
        modify' (const day)
        lift $ withElapsedTime ($location <> ":run") $ do
          ConnectInfo {..} <- fmap (^. psqlConn) ask
          tmp <- liftIO $ getTemporaryDirectory
          tm <- liftIO $ fmap round getPOSIXTime
          let args  = 
                [ "pg_dump_sql",
                  "DIR=" <> tmp,
                  "DB_PASS=" <> connectPassword, 
                  "DB_USER=" <> connectUser,
                  "DB_NAME=" <> connectDatabase,
                  "DB_HOST=" <> connectHost,
                  "TM=" <> show tm
                ]
          void $ liftIO $ readProcess "make" args mempty
          let file = "b-correspondent_" <> show tm <> ".sql"
          content <- liftIO $ readFile $ tmp <> "/" <> file
          let hash = mkHashMd5 content
          hasql <- fmap (^. hasqlDbPool) ask
          isOk <- transactionM hasql $ statement insert hash
          when isOk $ do 
            cfg <- fmap (^. google) ask
            mgr <- fmap (^. httpReqManager) ask
            for_ cfg $ \Google {..} -> do
              resp <- liftIO $ obtainAccessToken mgr googleTokenUrl googleTokenEmail googleTokenPk
              for_ resp $ \(AccessToken token) -> do
                k <- fmap (^. symmetricKeyBase) ask 
                let cipheredContent = cryptContent k content
                for_ cipheredContent $ \bs -> do
                  let part = [partBS (toS ("/b-correspondent_" <> show tm <> ".sql")) $ toS bs]
                  let hs = [(hAuthorization, toS ("Bearer " <> token)), (hContentType, "application/octet-stream")]
                  let onFailure = pure . Left . toS . show
                  let url = googleStorageUrl <> "/b-correspondent_" <> toS (show tm)
                  resp <- Request.safeMake @() url mgr hs Request.methodPost (Right part) onFailure
                  for_ resp $ const $
                    $(logTM) InfoS $ logStr @Text $
                      $location <> ":run ---> db is backed up successfully" 
                  logError resp
                logError cipheredContent
              logError resp

cryptContent :: ByteString -> String -> Either String Text
cryptContent bs content = 
  fromMaybe (Left "couldn't make IV from bs") $
    twofish128IV bs <&> \iv -> 
      bimap show (decodeUtf8 . B64.encode) $ 
        twofish128 (twofish128Key bs) iv $ toS content

logError x = whenLeft x $ \error -> $(logTM) ErrorS $ logStr @Text $ $location <> ":run ---> " <> toS error