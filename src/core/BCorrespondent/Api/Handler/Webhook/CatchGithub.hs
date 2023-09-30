{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module BCorrespondent.Api.Handler.Webhook.CatchGithub (catch) where

import BCorrespondent.Transport.Model.Github (Sha (..))
import BCorrespondent.Transport.Payload (Payload (..))
import BCorrespondent.Transport.Response (Response (Ok))
import Katip.Handler (KatipHandlerM, katipEnv, cache, ask)
import Control.Lens ((^.))
import Data.Aeson (eitherDecode, encode, Value (Object))
import Cache (Cache (..))
import Data.Foldable (for_)
import Data.String (fromString)
import BuildInfo (location)
import Katip
import Data.Either.Combinators (whenLeft)

catch :: Payload -> KatipHandlerM (Response ())
catch payload = do
  Cache {insert} <- fmap (^. katipEnv . cache) ask
  let val = 
        eitherDecode @Sha $ 
          encode $ 
            Object $ 
              getPayload payload
  whenLeft val $ \e -> 
    $(logTM) ErrorS $ 
      fromString $ 
        $location <> ", error: " <> e
  fmap Ok $ for_ val $ \Sha {..} -> undefined