{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module BCorrespondent.Config
  ( Config,
    Katip (..),
    Db,
    Swagger (..),
    Env (..),
    Cors (..),
    ServerError (..),
    Email (..),
    StdoutFormat (..),
    db,
    pass,
    port,
    database,
    host,
    user,
    poolN,
    tm,
    hasql,
    resPerStripe,
    katip,

    -- * load config
    load,
    path,
    verbosity,
    severity,
    stdoutFormat,
    env,
    accessKey,
    secretKey,
    logBucket,
    minio,
    bucketPrefix,
    swagger,
    serverConnection,
    cors,
    origins,
    serverError,
    webhook,
    jobFrequency,
    countryCodeFilePath,
    sourceTokenLife,

    -- * Iso
    isoEnv,
  )
where

import Control.Exception
import Control.Lens
import Data.Aeson
import Data.Aeson.TH.Extended
import Data.String.Conv
import Data.Swagger (ToSchema)
import qualified Data.Text as T
import Data.Yaml
import GHC.Generics
import TH.Mk
import Control.Monad.Catch (throwM)
import BuildInfo (location)

data Db = Db
  { dbHost :: !String,
    dbPort :: !Int,
    dbUser :: !String,
    dbPass :: !String,
    dbDatabase :: !String
  }
  deriving (Show)

data Swagger = Swagger {swaggerHost :: String, swaggerPort :: Maybe Int}
  deriving (Show)

data HasqlSettings = HasqlSettings
  { hasqlSettingsPoolN :: !Int,
    hasqlSettingsTm :: !Double,
    hasqlSettingsResPerStripe :: !Int
  }
  deriving (Show)

data Env = Prod | Dev deriving (Show, Eq)

mkEnumConvertor ''Env

instance FromJSON Env where 
  parseJSON = withText $location (pure . toEnv . toS)

data StdoutFormat = Json | Bracket deriving (Show, Eq)

mkEnumConvertor ''StdoutFormat

instance FromJSON StdoutFormat where
    parseJSON = withText $location (pure . toStdoutFormat . toS)

data Katip = Katip
  { katipPath :: !FilePath,
    katipSeverity :: !String,
    katipVerbosity :: !String,
    katipEnv :: !Env,
    katipStdoutFormat :: !StdoutFormat
  }
  deriving (Show)

data Minio = Minio
  { minioAccessKey :: !T.Text,
    minioSecretKey :: !T.Text,
    minioBucketPrefix :: !T.Text,
    minioHost :: !String,
    minioPort :: !String,
    minioLogBucket :: !String
  }
  deriving (Show)

newtype ServerConnection = ServerConnection {serverConnectionPort :: Int}
  deriving (Show)

newtype Cors = Cors {corsOrigins :: (Maybe [T.Text])} deriving (Show)

newtype ServerError = ServerError {serverErrorMk500 :: Bool}
  deriving stock (Generic)
  deriving newtype (FromJSON)
  deriving stock (Show)

newtype Email = Email T.Text
  deriving stock (Generic)
  deriving newtype (FromJSON, ToJSON)
  deriving stock (Show)
  deriving (ToSchema)

data Config = Config
  { configDb :: !Db,
    configSwagger :: !Swagger,
    configHasql :: !HasqlSettings,
    configKatip :: !Katip,
    configMinio :: !Minio,
    configServerConnection :: !ServerConnection,
    configCors :: !Cors,
    configServerError :: !ServerError,
    configJobFrequency :: !Int,
    configWebhook :: !T.Text,
    configCountryCodeFilePath :: !T.Text,
    configSourceTokenLife :: !Int
  }
  deriving (Show)

makeFields ''Config
makeFields ''Db
makeFields ''HasqlSettings
makeFields ''Katip
makeFields ''Minio
makeFields ''Swagger
makeFields ''ServerConnection
makeFields ''Cors

deriveFromJSON defaultOptions ''Db
deriveFromJSON defaultOptions ''HasqlSettings
deriveFromJSON defaultOptions ''Katip
deriveFromJSON defaultOptions ''Minio
deriveFromJSON defaultOptions ''ServerConnection
deriveFromJSON defaultOptions ''Cors
deriveFromJSON defaultOptions ''Swagger
deriveFromJSON defaultOptions ''Config

data ConfigException = ConfigFailed String
    deriving Show

instance Exception ConfigException

-- Load program configuration from file (server.yaml), or
-- raise YamlException and terminate program.
load :: FromJSON a => FilePath -> IO a
load path = decodeFileEither path >>= either (throwM . ConfigFailed . prettyPrintParseException) pure

