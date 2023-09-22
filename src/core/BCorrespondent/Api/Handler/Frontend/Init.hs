{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module BCorrespondent.Api.Handler.Frontend.Init (handle) where

import qualified BCorrespondent.Statement.Auth as Auth (checkToken)
import BCorrespondent.Transport.Response (Response, fromEither)
import BCorrespondent.Transport.Model.Frontend (Init, Sha (..), isJwtValid, defInit, shaXs, JWTStatus (..))
import BCorrespondent.Transport.Model.Auth (AuthToken (..))
import BCorrespondent.EnvKeys (key, repos)
import BCorrespondent.Auth (validateJwt)
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

data Error = Github GitHub.Error | Content404

instance Show Error where
  show (Github e) = "cannot find resource on github: " <> show e
  show Content404 = "content not found"

handle :: Maybe AuthToken -> KatipHandlerM (Response Init)
handle token = do 
  tokenResp <- for token $ \tk -> do
    key <- fmap (^. katipEnv . jwk) ask
    hasql <- fmap (^. katipEnv . hasqlDbPool) ask
    auth_logger <- katipAddNamespace (Namespace ["auth"]) askLoggerIO
    let checkToken = transaction hasql auth_logger . statement Auth.checkToken
    res <- liftIO $ validateJwt (defaultJWTSettings key) checkToken $ toS @Text $ coerce tk
    return $ case res of Left _ -> Invalid; _ -> Valid

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
          let mkKey | repo == "B-Correspondent-front" = "sha"
                    | otherwise = undefined
          fmap (second ((Sha mkKey) . GitHub.untagName . V.head . fmap GitHub.commitSha)) $
            GitHub.github (GitHub.OAuth (toS (val^.key))) query

  return $ fromEither $ (first (toS @_ @Text . show))  $ resp <&> \xs -> defInit { shaXs = xs, isJwtValid = fromMaybe Skip tokenResp }