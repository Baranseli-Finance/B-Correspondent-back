{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Server (Cfg (..), ServerM (..), run, populateCache, addServerNm) where

import BCorrespondent.Statement.Institution.Auth (Institution (..), fetchToken)
import qualified BCorrespondent.Job.Invoice as Job.Invoice
-- import qualified BCorrespondent.Job.History as Job.History
import qualified BCorrespondent.Job.Wallet as Job.Wallet
-- import qualified BCorrespondent.Job.Report as Job.Report
-- import qualified BCorrespondent.Job.Backup as Job.Backup
import qualified BCorrespondent.Job.Webhook as Job.Webhook
import qualified BCorrespondent.Job.Cache as Job.Cache
import qualified BCorrespondent.Job.Transaction as Job.Transaction
import BCorrespondent.Statement.Auth (CheckToken)
import BCorrespondent.Api
import BCorrespondent.EnvKeys (Sendgrid)
import qualified BCorrespondent.Api.Handler as Handler
import qualified BCorrespondent.Statement.Auth as Auth (checkToken)
import BCorrespondent.ServerM
import qualified BCorrespondent.Config as Cfg
import BCorrespondent.Transport.Error
import qualified BCorrespondent.Transport.Response as Response
import qualified Control.Concurrent.Async.Lifted as Async.Lifted
import Control.Exception
import BuildInfo
import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.RWS.Strict as RWS
import Data.Aeson
import Data.Coerce
import Data.Either.Combinators
import Data.Generics.Product.Fields
import Data.String.Conv
import qualified Data.Text as T
import Katip
import Katip.Handler
import Language.Haskell.TH.Syntax (Loc)
import qualified Network.HTTP.Types as H
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Header.Extended
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Parse
import Servant
import Servant.API.Generic
import Servant.Auth.Server
import Servant.Error.Formatters (formatters)
import Servant.Multipart
import Servant.Swagger.UI
import qualified Katip.Wai as Katip.Wai
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import qualified Network.Minio as Minio
import Database.Transaction (transactionIO, transactionM, statement)
import Data.UUID (UUID)
import Network.Wai.RateLimit.Backend
import Data.ByteString (ByteString)
import Network.Wai.RateLimit.Postgres (postgresBackend)
import Database.PostgreSQL.Simple.Internal
import qualified Data.Pool as Pool
import Data.Int (Int64)
import Data.String (fromString)
import Pretty (mkPretty)
import Servant.RawM.Server ()
import Cache (Cache (Cache, insert))
import Data.Foldable (for_)
import Network.Wai.Middleware.Prometheus (prometheus, PrometheusSettings)


data Cfg = Cfg
  { cfgHost :: !String,
    cfgSwaggerPort :: !(Maybe Int),
    cfgServerPort :: !Int,
    cfgCors :: !Cfg.Cors,
    cfgServerError :: !Cfg.ServerError,
    mute500 :: !(Maybe Bool),
    ns :: !Namespace,
    logEnv :: !LogEnv,
    manager :: !HTTP.Manager,
    minio :: !(Minio.MinioConn, T.Text),
    webhook :: !T.Text,
    jobFrequency :: !Int,
    sendgridCfg :: !(Maybe Sendgrid),
    psqlpool :: !(Pool.Pool Connection),
    freqBase :: !Int
  }

addServerNm :: forall a . KatipContextT ServerM a -> T.Text -> KatipContextT ServerM a
addServerNm ctx nm = katipAddNamespace (Namespace [nm]) ctx

run :: Cfg -> KatipContextT ServerM ()
run Cfg {..} = do
  logger <- askLoggerIO `addServerNm` "server"

  packageE <- liftIO getPackage
  whenLeft packageE $ (throwM . ErrorCall)
  let Right Package {..} = packageE

  $(logTM) DebugS $ fromString $ "server run on: " <> "http://127.0.0.1:" <> toS (show cfgServerPort)

  configKatipEnv <- lift ask
  let initCfg = do
        configEnv <- getLogEnv
        configCtx <- getKatipContext
        configNm <- getKatipNamespace
        return $ Config {..}
  cfg <- initCfg
  let withSwagger :: Proxy a -> Proxy (a :<|> SwaggerSchemaUI "swagger" "swagger.json")
      withSwagger _ = Proxy

  let hoistedServer =
        hoistServerWithContext
          (withSwagger api)
          (Proxy @'[CookieSettings, JWTSettings, (UUID, Int64) -> IO (Maybe CheckToken), Backend ByteString])
          (fmap fst . runKatipHandler cfg (State mempty))
          ( toServant Handler.handler
              :<|> swaggerSchemaUIServerT
                (swaggerHttpApi
                 cfgHost 
                 cfgSwaggerPort
                 packageVersion)
          )
  excep <- askLoggerIO `addServerNm` "exception"
  ctx_logger <- askLoggerIO `addServerNm` "context"
  req_logger <- askLoggerIO `addServerNm` "request"
  auth_logger <- askLoggerIO `addServerNm` "auth"

  let settings =
        Warp.defaultSettings
          & Warp.setPort cfgServerPort
          & Warp.setOnException 
            (logUncaughtException excep)
          & Warp.setOnExceptionResponse 
            (mk500Response (coerce cfgServerError) mute500)
          & Warp.setServerName 
            ("BCorrespondent api server, revision " <> $gitCommit)
          & Warp.setLogger (logRequest req_logger)
  let multipartOpts =
        (defaultMultipartOptions (Proxy @Tmp))
          { generalOptions = 
              setMaxRequestNumFiles 10 $
                -- the file size is bounded by 100 mb
                setMaxRequestFileSize 104_857_600 $
                  setMaxRequestKeyLength 100 
                    defaultParseRequestBodyOptions
          }

  let checkToken = transactionIO (katipEnvHasqlDbPool configKatipEnv) auth_logger . statement Auth.checkToken

  rateBackend <- liftIO $ postgresBackend psqlpool "rate_limit"

  let mkContext =
        multipartOpts :. 
        formatters :.
        defaultJWTSettings (configKatipEnv ^. jwk) :.
        checkToken :. 
        rateBackend :.
        defaultCookieSettings :.
        EmptyContext

  mware_logger <- askLoggerWithLocIO `addServerNm` "middleware"

  let jobs =
          [
          , Job.Invoice.forwardToPaymentProvider
          , Job.Wallet.withdraw
          , Job.Webhook.run
          , Job.Cache.removeExpiredItems
          , Job.Transaction.forward
          ]

  ctx <- getKatipContext

  jobsAsync <- mapM Async.Lifted.async $ zipWith uncurry jobs $ map ((freqBase, ) . (jobFrequency +)) [1, 3 .. ]

  serverAsync <-
    Async.Lifted.async $
      liftIO $ do
        Warp.runSettings settings $
          middleware cfgCors mware_logger $
            prometheus (def @PrometheusSettings) $
              Katip.Wai.runApplication
              (runKatipContextT logEnv ctx ns) $
                mkApplication $
                  serveWithContext (withSwagger api) mkContext hoistedServer
    
  asyncRes <- fmap snd $ Async.Lifted.waitAnyCatchCancel (serverAsync : jobsAsync)  `logExceptionM` ErrorS

  whenLeft asyncRes $ \reason -> do 
    $(logTM) EmergencyS $ 
      fromString $
        mkPretty mempty $ 
          "server has been terminated: " <> 
          show reason

middleware :: Cfg.Cors -> KatipLoggerLocIO -> Application -> Application
middleware cors log app = mkCors cors app

logUncaughtException :: KatipLoggerIO -> Maybe Request -> SomeException -> IO ()
logUncaughtException log reqm e =
  when (Warp.defaultShouldDisplayException e) $ do
    let withReq req = 
          log CriticalS $
            fromString $ 
              "\"" <> 
              toS (requestMethod req) <> " " 
              <> toS (rawPathInfo req) <> " " <> 
              toS (show (httpVersion req)) <> 
              "500 - " <> show e
    maybe (log CriticalS (logStr ("before request being handled" <> show e))) withReq reqm

mk500Response :: Bool -> Maybe Bool -> SomeException -> Response
mk500Response False _ error = 
  responseLBS status200 hs $ 
    encode @(Response.Response ()) $ 
      Response.Error (Just 500) $ 
        asError @T.Text (toS (show error))
  where hs = [ (H.hContentType, "application/json; charset=utf-8"), 
               (hAccessControlAllowOrigin, "*")
             ]
mk500Response True (Just True) error = 
  responseLBS status500 hs $ toS (show error)
  where hs = [ (H.hContentType, "text/plain; charset=utf-8"), 
               (hAccessControlAllowOrigin, "*") 
             ]
mk500Response True _  error = 
  responseLBS status200 hs $ 
    encode @(Response.Response ()) $ 
      Response.Error (Just 500) $ 
        asError @T.Text (toS (show error))
  where hs = [ (H.hContentType, "text/json; charset=utf-8"), 
               (hAccessControlAllowOrigin, "*") 
             ]

logRequest :: KatipLoggerIO -> Request -> Status -> Maybe Integer -> IO ()
logRequest log req _ _ = log InfoS (logStr (show req))

deriving instance Generic CorsResourcePolicy

mkCors :: Cfg.Cors -> Middleware
mkCors cfg_cors =
  cors $
    const $
      pure $
        simpleCorsResourcePolicy
          & field @"corsOrigins"
            .~ fmap ((,True) . map toS) (Cfg.corsOrigins cfg_cors)
          & field @"corsRequestHeaders"
            .~ [hAuthorization, hContentType, hOrigin]
          & field @"corsMethods"
            .~ simpleMethods
              <> [methodPut, methodPatch, methodDelete, methodOptions]
          & field @"corsIgnoreFailures" .~ True

askLoggerWithLocIO :: KatipContextT ServerM (Maybe Loc -> Severity -> LogStr -> IO ())
askLoggerWithLocIO = do
  ctx <- getKatipContext
  ns <- getKatipNamespace
  logEnv <- getLogEnv
  pure $ \loc sev msg ->
    runKatipT logEnv $
      logItem ctx ns loc sev msg

mkApplication :: Application -> Katip.Wai.ApplicationT (KatipContextT IO)
mkApplication hoistedApp =
  Katip.Wai.middleware DebugS $ \request send ->
    withRunInIO $ \toIO -> hoistedApp request (toIO . send)

populateCache :: Cache (KatipContextT ServerM) T.Text Value -> KatipContextT ServerM ()
populateCache Cache {insert} = do
  hasql <- fmap (^. hasqlDbPool) ask
  res <- transactionM hasql $ statement fetchToken [Elekse, Tochka]
  for_ res $ \(key, token) -> flip (insert key) True $ toJSON token