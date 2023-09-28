{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BCorrespondent.Transport.Model.Fs (Bucket (..)) where

import Data.Text (Text)
import Data.Swagger (ToParamSchema)
import GHC.Generics (Generic)
import Servant.API


newtype Bucket = Bucket Text
 deriving stock Generic
 deriving anyclass (ToParamSchema)
 deriving newtype (FromHttpApiData)