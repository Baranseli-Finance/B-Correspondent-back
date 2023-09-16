{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module BCorrespondent.Api.Handler.Auth.GenerateToken (controller) where

import BCorrespondent.Api.Handler.Utils (withError)
import BCorrespondent.Statement.Auth (getInstitutionId, insertInstToken)
import BCorrespondent.Transport.Model.Auth (AuthToken (..), InstitutionKey (..))
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Auth (generateJWT)
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

data Error = Inst404 | JWT | DB

instance Show Error where
  show Inst404 = "wrong key"
  show JWT = "jwt generation error"
  show DB = ""

controller :: InstitutionKey -> KatipHandlerM (Response AuthToken)
controller instKey = do 
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  key <- fmap (^. katipEnv . jwk) ask
  fmap ((`withError` AuthToken) .join) $ 
    transactionM hasql $ do 
      identm <- statement getInstitutionId $ coerce instKey
      fmap (maybeToRight Inst404) $
        for identm $ \ident -> do 
          res <- liftIO $ generateJWT key ident 3600
          fmap (join . first (const JWT)) $
            for res $ \(bs, uuid) -> 
              fmap (const (Right (toS bs))) $ 
                statement insertInstToken (ident, toS bs, uuid)