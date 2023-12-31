{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Katip.Handler
  ( Config (..),
    KatipHandlerM (..),
    KatipEnv (..),
    KatipLogger (..),
    KatipLoggerIO,
    KatipLoggerLocIO,
    State (..),
    Minio (..),

    -- * lens
    nm,
    ctx,
    env,
    katipEnv,
    terminal,
    httpReqManager,
    minio,
    bucketPrefix,
    hasqlDbPool,
    conn,
    jwk,
    webhook,
    github,
    frontEnvFilePath,
    cache,
    countryCode,
    tokenLife,
    templateDir,
    smtpCfg,
    psqlConn,
    google,
    symmetricKeyBase,
    rSAKey,
    backupBigDB,

    -- * run
    runKatipHandler,

    -- * re-export
    module R,

    -- * katip
    askLoggerIO,

    sendGrid,
  )
where

import BCorrespondent.EnvKeys
import Control.DeepSeq
import Control.Lens
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.RWS.Class
import qualified Control.Monad.RWS.Strict as RWS
import Control.Monad.Reader.Class as R
import Control.Monad.Time
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Crypto.JWT as Jose
import Data.Monoid.Colorful (Term)
import qualified Data.Pool as Pool
import qualified Data.Text as T
import "time" Data.Time
import qualified Hasql.Connection as Hasql
import Katip
import Katip.Monadic
import Language.Haskell.TH.Syntax
import Network.HTTP.Client
import qualified Network.Minio as Minio
import "sendgrid" OpenAPI.Common as SendGrid
import Servant.Server (Handler)
import Servant.Server.Internal.ServerError
import Data.Tuple.Extended (del3)
import Cache (Cache)
import Data.Aeson (Value)
import Database.PostgreSQL.Simple.Internal (ConnectInfo)
import Data.ByteString (ByteString)
import qualified Crypto.PubKey.RSA as RSA


type KatipLoggerIO = Severity -> LogStr -> IO ()

type KatipLoggerLocIO = Maybe Loc -> Severity -> LogStr -> IO ()

data KatipEnv = KatipEnv
  { katipEnvTerminal :: !Term,
    katipEnvHasqlDbPool :: !(Pool.Pool Hasql.Connection),
    katipEnvHttpReqManager :: !Manager,
    katipEnvMinio :: !Minio,
    katipEnvSendGrid :: !(Maybe (Sendgrid, SendGrid.Configuration)),
    katipEnvJwk :: !Jose.JWK,
    katipEnvWebhook :: !T.Text,
    katipEnvGithub :: !(Maybe Github),
    katipEnvFrontEnvFilePath :: !FilePath,
    katipEnvCache :: !(Cache KatipHandlerM T.Text Value),
    katipEnvCountryCode :: !T.Text,
    katipEnvTokenLife :: !Int,
    katipEnvTemplateDir :: !T.Text,
    katipEnvSmtpCfg :: !(Maybe SMTP),
    katipEnvPsqlConn :: !ConnectInfo,
    katipEnvGoogle :: !(Maybe Google),
    katipEnvSymmetricKeyBase :: !ByteString,
    katipEnvBackupBigDB :: !Bool,
    katipEnvRSAKey :: !RSA.RSAKey
  }

data Minio = Minio {minioConn :: !Minio.MinioConn, minioBucketPrefix :: !T.Text}

newtype KatipLogger = KatipWriter [String]
  deriving newtype (Monoid)
  deriving newtype (Semigroup)
  deriving newtype (NFData)

data Config = Config
  { configNm :: !Namespace,
    configCtx :: !LogContexts,
    configEnv :: !LogEnv,
    configKatipEnv :: !KatipEnv
  }

instance MonadTime Handler where
  currentTime = liftIO getCurrentTime

newtype State = State { getState :: [Int] }

newtype KatipControllerWriter = KatipControllerWriter [String]
  deriving newtype (Monoid)
  deriving newtype (Semigroup)

-- ServerM
newtype KatipHandlerM a = KatipHandlerM { unwrap :: RWS.RWST Config KatipControllerWriter State Handler a }
  deriving newtype (Functor)
  deriving newtype (Applicative)
  deriving newtype (Monad)
  deriving newtype (MonadIO)
  deriving newtype (MonadReader Config)
  deriving newtype (MonadState State)
  deriving newtype (MonadWriter KatipControllerWriter)
  deriving newtype (MonadBase IO)
  deriving newtype (MonadBaseControl IO)
  deriving newtype (MonadError ServerError)
  deriving newtype (MonadCatch)
  deriving newtype (MonadThrow)
  deriving newtype (MonadMask)
  deriving newtype (MonadTime)
  deriving newtype (MonadRWS Config KatipControllerWriter State)
  deriving newtype (MonadFail)

makeFields ''Config
makeFields ''KatipEnv
makeFields ''Minio

-- These instances get even easier with lenses!
instance Katip KatipHandlerM where
  getLogEnv = KatipHandlerM $ asks configEnv
  localLogEnv f (KatipHandlerM m) = KatipHandlerM (local (over env f) m)

instance KatipContext KatipHandlerM where
  getKatipContext = KatipHandlerM $ asks configCtx
  localKatipContext f (KatipHandlerM m) = KatipHandlerM (local (over ctx f) m)
  getKatipNamespace = KatipHandlerM $ asks configNm
  localKatipNamespace f (KatipHandlerM m) = KatipHandlerM (local (over nm f) m)

runKatipHandler :: Config -> State -> KatipHandlerM a -> Handler (a, State)
runKatipHandler cfg st app = fmap del3 $ RWS.runRWST (unwrap app) cfg st