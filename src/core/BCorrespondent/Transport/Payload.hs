{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveFoldable #-}

module BCorrespondent.Transport.Payload (Payload (..), valueToPayload) where

import Data.Aeson
import Data.Aeson.KeyMap
import Data.Proxy
import Data.Text (pack)
import Data.Typeable (typeRep)
import GHC.Generics
import Test.QuickCheck.Arbitrary
import Control.Lens
import Data.Swagger

-- | Swagger friendly wrapper over any JSON object
newtype Payload = Payload {getPayload :: Object}
  deriving stock (Eq)
  deriving stock (Show)
  deriving stock (Generic)
  deriving newtype (ToJSON)
  deriving newtype (FromJSON)

instance Arbitrary Payload where arbitrary = pure $ Payload empty

instance ToSchema Payload where
  declareNamedSchema _ =
    let name = Just (pack (show (typeRep (Proxy @Payload))))
    in pure $ 
        NamedSchema name $ 
          mempty
            & type_ ?~ SwaggerObject
            & properties .~mempty

valueToPayload :: Value -> Payload
valueToPayload (Object o) = Payload o
valueToPayload v = Payload $ singleton "value" v
