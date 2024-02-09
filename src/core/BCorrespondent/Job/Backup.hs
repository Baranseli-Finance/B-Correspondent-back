{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NumericUnderscores #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module BCorrespondent.Job.Backup (run) where


import BCorrespondent.ServerM (ServerM)
import BCorrespondent.Statement.Backup (insert)
import BCorrespondent.Job.Backup.Google (AccessToken (..), obtainAccessToken) 
import BCorrespondent.EnvKeys (Google (..))
import Katip
import BuildInfo (location)
import Control.Concurrent.Lifted (threadDelay)
import Control.Monad (when, void, forever)
import Control.Monad.Trans.State.Strict (evalStateT, get, modify')
import Control.Monad.Trans.Class (lift)
import Hash (mkHashMd5)
import System.Process (readProcess)
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple.Internal (ConnectInfo (..))
import Control.Lens ((^.))
import Katip.Handler (psqlConn, hasqlDbPool, google, httpReqManager, symmetricKeyBase, backupBigDB, ask)
import System.Directory (getTemporaryDirectory)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.Transaction (statement, transactionM)
import Data.Text (Text)
import qualified Data.Text as T 
import Data.Foldable (for_)
import Data.Either.Combinators (whenLeft)
import Data.String.Conv (toS)
import Network.HTTP.Client.MultipartFormData (partBS)
import qualified Request (safeMake, methodPost)
import Network.HTTP.Types.Header (hAuthorization, hContentType)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (length, splitAt, takeEnd)
import Crypto.Cipher.Symmetric (twofish128Key, twofish128IV, twofish128)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Base64 as B64
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime, localTimeOfDay, todHour)
import qualified Control.Parallel.Strategies as Par


run :: Int -> Int -> KatipContextT ServerM ()
run freqBase freq = do
  !hour <- liftIO getCurrentHour
  flip evalStateT hour $ do
    forever $ do
      threadDelay $ freq * freqBase
      conn <- fmap (^. psqlConn) ask
      go conn
  where
    go ConnectInfo {..} = do
      currHour <- get
      !hour <- liftIO getCurrentHour
      doBackup <- fmap (^. backupBigDB) ask
      when (hour /= currHour && doBackup) $ do
        modify' (const hour)
        lift $ do
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
          let hash = mkHashMd5 toS content
          hasql <- fmap (^. hasqlDbPool) ask
          isOk <- transactionM hasql $ statement insert hash
          when isOk $ do 
            cfg <- fmap (^. google) ask
            mgr <- fmap (^. httpReqManager) ask
            for_ cfg $ \Google {..} -> do
              resp <- liftIO $ obtainAccessToken mgr googleTokenUrl googleTokenEmail googleTokenPk
              for_ resp $ \(AccessToken token) -> do
                k <- fmap (^. symmetricKeyBase) ask
                let cipheredContent = cryptContent k $ toS content
                for_ cipheredContent $ \bs -> do
                  let part = [partBS (toS ("/b-correspondent_" <> show tm <> ".sql")) $ toS bs]
                  let hs = 
                        [(hAuthorization, toS ("Bearer " <> token)), 
                         (hContentType, "application/octet-stream")
                        ]
                  let onFailure = pure . Left . toS . show
                  let url = googleStorageUrl <> "/b-correspondent_" <> toS (show tm)
                  resp <- Request.safeMake @() url mgr hs Request.methodPost (Right part) onFailure
                  for_ resp $ const $
                    $(logTM) InfoS $ logStr @Text $
                      $location <> ":run ---> db is backed up successfully" 
                  whenLeft resp $ \error -> $(logTM) ErrorS $ logStr @Text $ $location <> ":run ---> " <> toS error
                whenLeft cipheredContent $ \error -> $(logTM) ErrorS $ logStr @Text $ $location <> ":run ---> " <> toS error
              whenLeft resp $ \error -> $(logTM) ErrorS $ logStr @Text $ $location <> ":run ---> " <> toS error

getCurrentHour :: IO Int
getCurrentHour = do 
  now <- getCurrentTime
  timezone <- getCurrentTimeZone
  return $ todHour $ localTimeOfDay $ utcToLocalTime timezone now

maxSize = 5_000_000 :: Int

cryptContent :: ByteString -> ByteString -> Either String Text
cryptContent bs content 
  | B.length content < maxSize = cryptContentImpl bs content
  | otherwise =
    let n = B.length content `div` maxSize
        chunks = mkChunks n content
        -- less then maxSize therefore we can easily apply to it 
        -- the brute-force approach mainly cryptContentImpl
        rest = B.length content `mod` maxSize
    in do
         xs <- cryptInPar bs chunks
         x <- cryptContentImpl bs $ B.takeEnd rest content
         pure $ T.concat $ xs <> [x]

cryptContentImpl :: ByteString -> ByteString -> Either String Text
cryptContentImpl bs content = 
  fromMaybe (Left "couldn't make IV from bs") $
    twofish128IV bs <&> \iv -> 
      bimap show (decodeUtf8 . B64.encode) $ 
        twofish128 (twofish128Key bs) iv content

mkChunks :: Int -> ByteString -> [ByteString]
mkChunks n bs = 
  let (x, ys) = B.splitAt maxSize bs 
  in go (n - 1) [x] ys
  where
    go 0 rs _ = reverse rs
    go i rs ys = 
      let (r, ys') = B.splitAt maxSize ys 
      in go (n - 1) (r : rs) ys'

cryptInPar :: ByteString -> [ByteString] -> Either String [Text]
cryptInPar bs = sequence . Par.parMap (Par.rparWith Par.rdeepseq) (cryptContentImpl bs)