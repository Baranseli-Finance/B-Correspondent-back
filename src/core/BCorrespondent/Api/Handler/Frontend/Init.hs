{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module BCorrespondent.Api.Handler.Frontend.Init (handle) where

import qualified BCorrespondent.Statement.Auth as Auth (checkToken)
import BCorrespondent.Transport.Response (Response, fromEither)
import BCorrespondent.Transport.Model.Frontend (Init, Sha (..), isJwtValid, logLevel, toTelegram, defInit, shaXs, JWTStatus (..), LogLevel)
import BCorrespondent.Transport.Model.Auth (AuthToken (..))
import BCorrespondent.EnvKeys (key, repos)
import BCorrespondent.Auth (validateJwt, AuthenticatedUser (..), account, AccountType (Institution))
import Database.Transaction (transaction, statement)
import Data.String.Conv (toS)
import Data.Coerce (coerce)
import Servant.Auth.Server (defaultJWTSettings)
import Control.Lens
import Katip
import Katip.Handler
import Data.Traversable (for)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified GitHub as GitHub
import Control.Concurrent.Async (forConcurrently)
import Control.Monad (join)
import Data.Either.Combinators (maybeToRight)
import Data.String (fromString)
import Data.Bifunctor
import Data.Aeson (eitherDecodeFileStrict, FromJSON)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)

data Error = Github GitHub.Error | Content404

instance Show Error where
  show (Github e) = "cannot find resource on github: " <> show e
  show Content404 = "content not found"

data FrontEnvs = 
     FrontEnvs 
     { frontEnvsLogLevel :: !LogLevel, 
       frontEnvsToTelegram :: !Bool 
     }
    deriving stock (Generic)
    deriving
      (FromJSON)
      via WithOptions 
         '[FieldLabelModifier 
           '[UserDefined ToLower,
             UserDefined (StripConstructor FrontEnvs)]]
      FrontEnvs

handle :: Maybe AuthToken -> KatipHandlerM (Response Init)
handle token = do
  
  path <- fmap (^. katipEnv . frontEnvFilePath) ask
  Right FrontEnvs {frontEnvsLogLevel, frontEnvsToTelegram} <- liftIO $ eitherDecodeFileStrict @FrontEnvs path 

  tokenResp <- for token $ \tk -> do
    key <- fmap (^. katipEnv . jwk) ask
    hasql <- fmap (^. katipEnv . hasqlDbPool) ask
    auth_logger <- katipAddNamespace (Namespace ["auth"]) askLoggerIO
    let checkToken = transaction hasql auth_logger . statement Auth.checkToken
    let analyse (Left _) = Invalid
        analyse (Right AuthenticatedUser {account = Institution}) = Invalid
        analyse _ = Valid
    liftIO $ fmap analyse $ validateJwt (defaultJWTSettings key) checkToken $ toS @Text $ coerce tk

  github <- fmap (^. katipEnv . github) ask
  resp <- fmap (join . maybeToRight Content404) $
    for github $ \val -> liftIO $ do
      fmap (first Github . sequence) $
        forConcurrently (val ^. repos) $ \repo -> do
          let query =
                GitHub.commitsForR
                  "Baranseli-Finance"
                  (fromString (toS repo))
                  (GitHub.FetchAtLeast 1)
          fmap (second ((Sha repo) . GitHub.untagName . V.head . fmap GitHub.commitSha)) $
            GitHub.github (GitHub.OAuth (toS (val^.key))) query

  return $ fromEither $ (first (toS @_ @Text . show)) $ resp <&> \xs -> 
    defInit { 
      shaXs = xs, 
      isJwtValid = fromMaybe Skip tokenResp, 
      logLevel = frontEnvsLogLevel, 
      toTelegram = frontEnvsToTelegram }