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

data Error = Code | JWT

instance Show Error where
  show Code = "code wrong"
  show JWT = "jwt generation error"
 
handle :: AuthCode -> KatipHandlerM (Response AuthToken)
handle (AuthCode code hash) = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  key <- fmap (^. katipEnv . jwk) ask
  let mkToken ident login = liftIO $ Auth.generateJWT key ident (Just login) 2_592_000
  res <- fmap join $ transactionM hasql $ do
    identm <- statement Auth.getUserCredByCode (code, hash)
    fmap (maybeToRight Code) $
      for identm $ \(Auth.UserCred {..}) -> do
        tokene <- mkToken userCredIdent userCredLogin
        fmap (first (const JWT)) $
          for tokene $ \(tokenbs, uuid) -> do
            let token = tokenbs ^. bytesLazy . from textbs
            statement Auth.insertToken (userCredIdent, token, uuid, hash) $> token
  return $ withError res AuthToken