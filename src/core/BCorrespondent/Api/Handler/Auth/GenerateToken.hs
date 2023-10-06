{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Auth.GenerateToken (handle) where

import BCorrespondent.Api.Handler.Utils (withError)
import BCorrespondent.Statement.Auth 
      (getInstitutionCreds, 
       insertInstToken, 
       InstitutionCreds (..), 
       CheckToken (..), 
       AccountType (Institution)
      )
import BCorrespondent.Transport.Model.Auth (AuthToken (..), InstitutionKey (..))
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Auth (generateJWT, validateJwt, Role (None))
import Katip.Handler
import Control.Lens ((^.))
import Data.Coerce (coerce)
import Database.Transaction (transactionM, statement)
import Data.Either.Combinators (maybeToRight)
import Data.Traversable (for)
import Control.Monad (join)
import Control.Monad.IO.Class
import Data.Bifunctor (first)
import Data.String.Conv (toS)
import Servant.Auth.Server (defaultJWTSettings)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

data Error = Inst404 | JWT

instance Show Error where
  show Inst404 = "wrong key"
  show JWT = "jwt generation error"

handle :: InstitutionKey -> KatipHandlerM (Response AuthToken)
handle instKey = do 
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  tl <- fmap (^. katipEnv . tokenLife) ask
  key <- fmap (^. katipEnv . jwk) ask
  let mkToken ident = do
       res <- liftIO $ generateJWT key ident Nothing $ fromIntegral tl
       fmap (join . first (const JWT)) $
         for res $ \(bs, uuid) -> 
           fmap (const (Right (toS bs))) $ 
             statement insertInstToken (ident, toS bs, uuid)
  fmap ((`withError` AuthToken) .join) $ 
    transactionM hasql $ do 
      credm <- statement getInstitutionCreds $ coerce instKey
      fmap (maybeToRight Inst404) $
        for credm $ \InstitutionCreds {..} ->
          case institutionCredsJwt of 
            Nothing -> mkToken institutionCredsIdent
            Just value -> do
              let isValid = fromMaybe undefined institutionCredsIsValid
              let checkToken = 
                    CheckToken 
                    { checkTokenIsValid = isValid, 
                      checkTokenAccountType = Institution,
                      checkTokenRole = Just $ V.singleton $ toS $ show None
                    }
              res <- liftIO $ validateJwt (defaultJWTSettings key) (const $ pure (Just checkToken)) $ toS value
              case res of 
                Right _ -> pure $ Right value
                Left _ -> mkToken institutionCredsIdent