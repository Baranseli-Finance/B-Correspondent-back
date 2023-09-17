{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}

module BCorrespondent.Api.Handler.Auth.Login (handle) where

import BCorrespondent.Api.Handler.Utils (withError)
import qualified BCorrespondent.Auth as Auth
import qualified BCorrespondent.Statement.Auth as Auth
import BCorrespondent.Transport.Model.Auth (AuthToken (..), Credentials (email, password))
import BCorrespondent.Transport.Response (Response)
import Control.Lens
import Control.Lens.Iso.Extended
import Control.Monad (join)
import Control.Monad.IO.Class
import Data.Bifunctor (first)
import Data.Either.Combinators (maybeToRight)
import Data.Traversable (for)
import Database.Transaction
import Katip.Handler

data Error = User404 | JWT | WrongPass

instance Show Error where
  show User404 = "user wrong"
  show JWT = "jwt generation error"
  show WrongPass = "wrong pass"

handle :: Credentials -> KatipHandlerM (Response AuthToken)
handle cred = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  key <- fmap (^. katipEnv . jwk) ask
  let mkToken ident = liftIO $ Auth.generateJWT key ident 2_592_000
  res <- fmap join $ transactionM hasql $ do
    identm <- statement Auth.getUserIdByEmail (email cred)
    fmap (maybeToRight User404) $
      for identm $ \ident -> do
        tokene <- mkToken ident
        fmap (join . first (const JWT)) $
          for tokene $ \(tokenbs, uuid) -> do
            let token = tokenbs ^. bytesLazy . from textbs
            res <- statement Auth.insertToken (email cred, password cred, token, uuid)
            return $
              if res
                then Right token
                else Left WrongPass
  return $ withError res AuthToken