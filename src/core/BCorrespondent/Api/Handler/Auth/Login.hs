{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}

module BCorrespondent.Api.Handler.Auth.Login (handle) where

import BCorrespondent.Api.Handler.Utils (withError)
import qualified BCorrespondent.Auth as Auth
import qualified BCorrespondent.Statement.Auth as Auth
import BCorrespondent.Transport.Model.Auth (AuthCode (..), AuthToken (..))
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
import Data.Functor (($>))

data Error = User404 | JWT

instance Show Error where
  show User404 = "user wrong"
  show JWT = "jwt generation error"
 
handle :: AuthCode -> KatipHandlerM (Response AuthToken)
handle (AuthCode code) = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  key <- fmap (^. katipEnv . jwk) ask
  let mkToken ident email = liftIO $ Auth.generateJWT key ident (Just email) 2_592_000
  res <- fmap join $ transactionM hasql $ do
    identm <- statement Auth.getUserCredByCode code
    fmap (maybeToRight User404) $
      for identm $ \(Auth.UserCred {..}) -> do
        tokene <- mkToken userCredIdent userCredEmail
        fmap (first (const JWT)) $
          for tokene $ \(tokenbs, uuid) -> do
            let token = tokenbs ^. bytesLazy . from textbs
            statement Auth.insertToken (userCredIdent, token, uuid) $> token
  return $ withError res AuthToken