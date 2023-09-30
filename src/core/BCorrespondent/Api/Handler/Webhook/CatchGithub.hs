{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BCorrespondent.Api.Handler.Webhook.CatchGithub (catch) where

import qualified BCorrespondent.Transport.Model.Frontend as Front
import BCorrespondent.Transport.Model.Github (Sha (..))
import BCorrespondent.Transport.Payload (Payload (..))
import BCorrespondent.Transport.Response (Response (Ok))
import Katip.Handler (KatipHandlerM, katipEnv, cache, ask)
import Control.Lens ((^.))
import Data.Aeson (eitherDecode, encode, Value (Object), toJSON)
import Cache (Cache (..))
import Data.Foldable (for_)
import Data.String (fromString)
import BuildInfo (location)
import Katip
import Data.Either.Combinators (whenLeft)
import Data.Maybe (fromMaybe)
import Data.Functor ((<&>))

catch :: Payload -> KatipHandlerM (Response ())
catch payload = do
  let val = 
        eitherDecode @Sha $ 
          encode $ 
            Object $ 
              getPayload payload
  whenLeft val $ \e -> 
    $(logTM) ErrorS $ 
      fromString $ 
        $location <> ", error: " <> e
  fmap Ok $ for_ val $ \Sha {..} -> do
    Cache {insert, get} <- fmap (^. katipEnv . cache) ask
    oldXsE <- get "github" <&> 
        fromMaybe (Right mempty) 
      . fmap (eitherDecode . encode)
    for_ oldXsE $ \oldXs -> do
      let val = 
            Front.Sha 
            { Front.shaKey = 
              shaRepo, 
              Front.shaValue = 
              shaValue
            }
      let newXs = toJSON $ modifySha oldXs val
      insert "github" newXs

modifySha [] val = [val]
modifySha (x:xs) v 
  | Front.shaKey x == Front.shaKey v = v:xs
  | otherwise = x : modifySha xs v