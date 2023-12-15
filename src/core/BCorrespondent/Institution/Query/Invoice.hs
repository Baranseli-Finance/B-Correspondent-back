{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module BCorrespondent.Institution.Query.Invoice (Response (..), path) where

import Data.Aeson.Generic.DerivingVia (WithOptions (..), DefaultOptions)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Data.Time.Clock (UTCTime)
import Data.Text (Text)


path :: Text
path = "invoice"

data Response = Response { acceptedAt :: UTCTime }
    deriving stock (Generic, Show)
    deriving (FromJSON) via WithOptions DefaultOptions Response