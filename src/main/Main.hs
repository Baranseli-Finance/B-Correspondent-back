{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PackageImports #-}

module Main (main) where

import qualified BCorrespondent.Server as Server
import BCorrespondent.Config
import BCorrespondent.EnvKeys
import BCorrespondent.ServerM (ServerState (..), ServerM)
import BuildInfo (gitCommit)
import qualified Cfg.SendGrid as SendGrid
import Control.Applicative ((<|>))
import Control.Exception
import Control.Lens hiding (Unwrapped, Wrapped)
import Control.Lens.Iso.Extended
import Control.Monad
import Control.Monad.RWS.Strict (evalRWST)
import Data.Char (isUpper, toLower)
import Data.Foldable (for_)
import Data.Maybe
import Data.Monoid.Colorful (hGetTerm)
import qualified Data.Pool as Pool
import Data.String
import Data.Time.Clock.System
import Data.Traversable (for)
import GHC.Read
import qualified Hasql.Connection as HasqlConn
import Katip
import Katip.Scribes.Minio as Scribes.Minio
import qualified Katip.Scribes.Telegram as Scribes.Telegram
import Katip.Handler hiding (webhook)
import Network.HTTP.Client
  ( ManagerSettings
      ( managerConnCount,
        managerResponseTimeout
      ),
    responseTimeoutMicro,
  )
import qualified Network.HTTP.Client.TLS as Http
import qualified Network.Minio as Minio
import Options.Generic
import Pretty
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix
import System.IO
import Text.ParserCombinators.ReadPrec (pfail)
import qualified Text.Read.Lex as L
import Crypto.JOSE.JWK (JWK)
import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as B
import Data.Either.Combinators (whenLeft)
import Data.Aeson (eitherDecode')
import Data.String.Conv (toS)
import Database.PostgreSQL.Simple.Internal (ConnectInfo (..), connect, close)
import BuildInfo (getSystemInfo)
import qualified Cache.MVar as MemCache
import qualified Cache.PostgreSQL as SqlCache
import qualified Data.ByteString.Base64 as B64 
import Data.Text.Encoding (encodeUtf8)
import qualified Prometheus as Prometheus (register)
import qualified Prometheus.Metric.GHC as GHC.Prometheus (ghcMetrics)
import Text.Read (readEither)
import qualified "b-correspondent" Crypto.PubKey.RSA as RSA
import Data.Bifunctor (first)



data PrintCfg = Y | N deriving stock (Generic)

instance Show PrintCfg where
  show Y = "y"
  show N = "n"

instance Read PrintCfg where
  readPrec =
    parens
      ( do
          L.Ident s <- lexP
          case s of
            "y" -> return Y
            "n" -> return N
            _ -> pfail
      )

instance ParseField PrintCfg

data Cmd w = Cmd
  { cfgPath :: w ::: FilePath <?> "config file path",
    localhost :: w ::: Maybe String <?> "override db host if needed, used along with port",
    localport :: w ::: Maybe Int <?> "override db port if needed",
    pathToKatip :: w ::: Maybe FilePath <?> "path to katip log",
    pathToJwk :: w ::: FilePath <?> "path to jwk",
    pathToSymmetricBase :: w ::: FilePath <?> "path to ByteString upon which Twofish128 is made",
    pathToRsa :: w ::: FilePath <?> "path to RSA key",
    minioHost :: w ::: Maybe String <?> "minio host",
    minioPort :: w ::: Maybe String <?> "minio port",
    minioAccessKey :: w ::: String <?> "minio access key",
    minioSecretKey :: w ::: String <?> "minio secret key",
    swaggerHost :: w ::: Maybe String <?> "swagger host",
    swaggerPort :: w ::: Maybe Int <?> "swagger port",
    serverPort :: w ::: Maybe Int <?> "server port",
    printCfg :: w ::: Maybe PrintCfg <?> "whether config be printed",
    envPath :: w ::: Maybe FilePath <?> "file for storing sensitive data. it's used only in deployment",
    mute500 :: w ::: Maybe Bool <?> "how to render 500 error",
    bCorrespondentDbUser :: w ::: String <?> "db user",
    bCorrespondentDbPass :: w ::: String <?> "db pass",
    bCorrespondentDatabase :: w ::: String <?> "database",
    frontEnvFilePath :: w ::: String <?> "path to frontend envs",
    extraLog :: w ::: Bool <?> "whether to commit log to an additional source (file server, telegram, etc.)",
    freqBase :: w ::: Int 
  }
  deriving stock (Generic)

deriving instance Show (Cmd Unwrapped)

instance ParseRecord (Cmd Wrapped) where
  parseRecord =
    parseRecordWithModifiers
      defaultModifiers
        { fieldNameModifier = toSnake
        }

-- |
--   Convert CamelCased or mixedCases 'String' to a 'String' with underscores,
--   the \"snake\" 'String'.
--   It splits an input value to chunks by 'isUpper' predicate,
--   then adds underscores to each element except the first.
--   Finally concats the result and convers it downcase.
toSnake :: String -> String
toSnake = map toLower . concat . underscores . splitR isUpper
  where
    underscores [] = []
    underscores (h : t) = h : map ('_' :) t
    splitR _ [] = []
    splitR p s =
      let go m s' =
            case break p s' of
              (b', []) -> [m : b']
              (b', x : xs) -> (m : b') : go x xs
       in case break p s of
            (b, []) -> [b]
            ([], h : t) -> go h t
            (b, h : t) -> b : go h t

main :: IO ()
main = do
  void $ Prometheus.register GHC.Prometheus.ghcMetrics

  cmd@Cmd {..} <- unwrapRecord "BCorrespondent"
  print "------ Cmd: start ------"
  pPrint cmd
  print "------ Cmd: end ------"

  -- at this initialisation step we have to obtain sensitive data from env
  envKeys <- fmap join $ for envPath $ \path -> do
    cond <- doesFileExist path
    if cond
      then fmap Just $ BCorrespondent.Config.load @EnvKeys path
      else return Nothing

  print "------ EnvKeys: start ------"
  pPrint envKeys
  print "------ EnvKeys: end ------"

  rawCfg <- BCorrespondent.Config.load @BCorrespondent.Config.Config cfgPath
  let cfg =
        rawCfg
          & db . host %~ (`fromMaybe` localhost)
          & db . port %~ (`fromMaybe` localport)
          & db . user .~ bCorrespondentDbUser
          & db . pass .~ bCorrespondentDbPass
          & db . database .~ bCorrespondentDatabase
          & katip . path %~ (\path -> maybe path (</> path) pathToKatip)
          & BCorrespondent.Config.minio . host %~ (`fromMaybe` minioHost)
          & BCorrespondent.Config.minio . port %~ (`fromMaybe` minioPort)
          & BCorrespondent.Config.minio . accessKey .~ toS minioAccessKey
          & BCorrespondent.Config.minio . secretKey .~ toS minioSecretKey
          & swagger . host %~ (`fromMaybe` swaggerHost)
          & swagger . port %~ (flip (<|>) swaggerPort)
          & serverConnection . port %~ (`fromMaybe` serverPort)

  for_ printCfg $
    \case
      Y ->
        do
          print "------ Cfg: start ------"
          pPrint cfg
          print "------ Cfg: end ------"
      N -> pure ()

  -- at this initialisation step we have to put sensitive data into the config

  term <- hGetTerm stdout
  hSetBuffering stdout NoBuffering

  let mkRawConn x =
        HasqlConn.settings
          (x ^. host . stext . textbs)
          (x ^. port . to fromIntegral)
          (x ^. BCorrespondent.Config.user . stext . textbs)
          (x ^. pass . stext . textbs)
          (x ^. database . stext . textbs)

  hasqlpool <-
    Pool.newPool $
      Pool.defaultPoolConfig
      (do connRes <- HasqlConn.acquire (mkRawConn (cfg ^. db))
          case connRes of Left e -> error $ show e; Right conn -> pure conn
      )
      HasqlConn.release
      (cfg^.hasql.tm)
      (cfg^.hasql.resPerStripe)


  let psqlConnInfo = 
        ConnectInfo { 
          connectHost = cfg^.db.host,
          connectPort = cfg^.db^.port.to fromIntegral,
          connectUser =  cfg^.db.BCorrespondent.Config.user,
          connectPassword = cfg^.db.pass,
          connectDatabase  = cfg^.db.database }

  psqlpool <-
    Pool.newPool $
      Pool.defaultPoolConfig
      (connect psqlConnInfo)
      close
      (cfg^.hasql.tm)
      (cfg^.hasql.resPerStripe)

  std <-
    mkHandleScribeWithFormatter
      (if cfg^.katip.stdoutFormat == Json 
       then jsonFormat 
       else bracketFormat)
      ColorIfTerminal
      stdout
      (permitItem (cfg ^. katip . severity . from stringify))
      (cfg ^. katip . verbosity . from stringify)
  tm <- fmap systemSeconds getSystemTime
  let katipFilePath = cfg ^. katip . path <> "/" <> show tm <> ".log"
  createDirectoryIfMissing True $ cfg ^. katip . path
  fileHdl <- openFile katipFilePath AppendMode

  mapM_ (`hSetEncoding` utf8) [stdout, stderr, fileHdl]

  let mkNm = Namespace [toS getSystemInfo, toS ("commit: <" ++ $(gitCommit) ++ ">")]
  init_env <- initLogEnv mkNm (cfg ^. katip . BCorrespondent.Config.env . isoEnv . stext . coerced)

  file <-
    mkHandleScribe
      (ColorLog True)
      fileHdl
      (permitItem (cfg ^. katip . severity . from stringify))
      (cfg ^. katip . verbosity . from stringify)

  manager <-
    Http.newTlsManagerWith
      Http.tlsManagerSettings
        { managerConnCount = 50,
          managerResponseTimeout =
            responseTimeoutMicro (5 * 1_000_000)
        }

  minioEnv <-
    flip Minio.mkMinioConn manager $
      Minio.setCreds
        ( Minio.CredentialValue
            (fromString (cfg^.BCorrespondent.Config.minio.accessKey.from stext))
            (fromString (cfg^.BCorrespondent.Config.minio.secretKey.from stext))
            Nothing
        )
        (fromString (cfg ^. BCorrespondent.Config.minio . host <> ":" <> cfg ^. BCorrespondent.Config.minio . port))

  telegramScribe <- 
    for (envKeys >>= envKeysTelegram) $ \tel -> 
      Scribes.Telegram.mkScribe manager tel (permitItem WarningS) (cfg ^. katip . verbosity . from stringify)

  minioScribe <-
    Scribes.Minio.mkScribe
      minioEnv
      (cfg ^. BCorrespondent.Config.minio . logBucket . stext)
      (permitItem (cfg ^. katip . severity . from stringify))
      (cfg ^. katip . verbosity . from stringify)

  let env = do
              env' <- registerScribe "stdout" std defaultScribeSettings init_env
              if extraLog then do
                env'' <- registerScribe "file" file defaultScribeSettings env'
                env''' <- registerScribe "minio" minioScribe defaultScribeSettings env''
                fmap (fromMaybe env''') $ 
                  for telegramScribe $ \scribe ->
                    registerScribe "telegram" scribe defaultScribeSettings env'''
              else return env'
 
  unEnv <- env

  print "------ katip scribes: start ------"
  print $ Map.keys $ unEnv^.logEnvScribes
  print "------ katip scribes: end ------"

  print "------ server is about to run --------"

  let serverCfg =
        Server.Cfg
        { cfgHost = cfg ^. swagger . host . coerced,
          cfgSwaggerPort = cfg ^. swagger . port,
          cfgServerPort = cfg ^. serverConnection . port,
          cfgCors = cfg ^. cors,
          cfgServerError = cfg ^. serverError,
          mute500 = mute500,
          ns = mkNm ,
          logEnv = unEnv,
          manager = manager,
          minio = (minioEnv, cfg ^. BCorrespondent.Config.minio . BCorrespondent.Config.bucketPrefix),
          webhook = cfg^.webhook,
          jobFrequency = cfg^.jobFrequency,
          sendgridCfg = envKeys >>= envKeysSendgrid,
          psqlpool = psqlpool,
          freqBase = freqBase
        }

  cache <- MemCache.init
  -- not used
  _ <- SqlCache.init @(KatipContextT ServerM) @() hasqlpool

  jwke <- liftIO $ fmap (eitherDecode' @JWK) $ B.readFile pathToJwk
  symmetricKeyBasee <- fmap (B64.decode . encodeUtf8 . toS) $ B.readFile pathToSymmetricBase
  rsaKeye <- fmap (first (const "cannot read RSA") . readEither @RSA.RSAKey) $ readFile pathToRsa
 
  keysRes <- for ((,,) <$> jwke <*> symmetricKeyBasee <*> rsaKeye) $ \(jwk, symmetricKeyBase, rsaKey)  -> do

    print "--------- jwk ------------"
    putStrLn $ (take 200 (show jwk)) <> ".... }"

    print "--------- rsa ------------"
    putStrLn $ (take 200 (show rsaKey)) <> ".... }"

    let katipMinio = Minio minioEnv (cfg ^. BCorrespondent.Config.minio . BCorrespondent.Config.bucketPrefix)
    let katipEnv = 
          KatipEnv 
          { katipEnvTerminal = term,
            katipEnvHasqlDbPool = hasqlpool,
              katipEnvHttpReqManager = manager,
              katipEnvMinio = katipMinio,
              katipEnvSendGrid = 
                envKeys >>= envKeysSendgrid <&> \sendgrid -> 
                  (sendgrid, SendGrid.configure (sendgrid^.url) (sendgrid^.key) ),
              katipEnvJwk = jwk,
              katipEnvWebhook = cfg^.webhook,
              katipEnvGithub = envKeys >>= envKeysGithub,
              katipEnvFrontEnvFilePath = frontEnvFilePath,
              katipEnvCache = cache,
              katipEnvCountryCode = cfg^.countryCodeFilePath,
              katipEnvTokenLife = cfg^.sourceTokenLife,
              katipEnvTemplateDir = cfg^.BCorrespondent.Config.templateDir,
              katipEnvSmtpCfg = envKeys >>= envKeysSmtp,
              katipEnvPsqlConn = psqlConnInfo,
              katipEnvGoogle = envKeys >>= envKeysGoogle,
              katipEnvSymmetricKeyBase = symmetricKeyBase,
              katipEnvBackupBigDB = cfg^.BCorrespondent.Config.backupBigDB,
              katipEnvRSAKey = rsaKey
          }

    serverCache <- MemCache.init
    let def = ServerState 0 serverCache
    let shutdownMsg = print "------ server is shut down --------"
    let runKatip le = 
          runKatipContextT le (mempty @LogContexts) mempty $ 
            flip Server.addServerNm "server" $ 
              Server.populateCache serverCache >> Server.run serverCfg
    bracket env (flip (>>) shutdownMsg . closeScribes) $ void . (\serverM -> evalRWST (Server.runServerM serverM) katipEnv def) . runKatip

  whenLeft keysRes $ print . ((<>) "jwk, cipher key or rsa decode error ---> ")