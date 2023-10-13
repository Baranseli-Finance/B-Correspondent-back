{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module BCorrespondent.Api.Handler.Frontend.User.GetTimelineTransaction (handle) where

import BCorrespondent.Transport.Response (Response (Error))
import BCorrespondent.Statement.Dashboard (getTransaction)
import BCorrespondent.Transport.Id (Id (..))
import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Api.Handler.Utils (withError)
import BCorrespondent.Transport.Error (asError)
import Database.Transaction (transactionM, statement)
import BCorrespondent.Transport.Model.Frontend (TimelineTransaction)
import BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution)
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, ask)
import Control.Lens ((^.))
import Data.Tuple.Extended (snocT, del1)
import Data.Coerce (coerce)
import Data.String (fromString)
import Data.Text (Text)

handle 
  :: Auth.AuthenticatedUser 'Auth.Reader 
  -> Id "transaction" 
  -> KatipHandlerM 
     (Response TimelineTransaction)
handle user ident = 
  checkInstitution user $ \x -> do 
    hasql <- fmap (^. katipEnv . hasqlDbPool) ask
    let error404 = 
          Error (Just 404) $ 
            asError $ 
              fromString @Text "transaction doesn't exist"
    fmap (maybe error404 (`withError` id)) $ 
      transactionM hasql $ 
        statement getTransaction $ 
          del1 $ snocT (coerce ident) x