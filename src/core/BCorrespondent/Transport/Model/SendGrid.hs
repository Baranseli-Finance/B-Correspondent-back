{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module BCorrespondent.Transport.Model.SendGrid (DeliveryWStatus (..), DeliveryEvent (..)) where

-- https://docs.sendgrid.com/for-developers/tracking-events/event

import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Generic.DerivingVia
import GHC.Generics

data DeliveryWStatus = Bounce | Blocked | Delivered
     deriving stock (Generic, Show, Eq, Read, Ord)
     deriving (FromJSON, ToJSON)
      via WithOptions
          '[ConstructorTagModifier 
            '[UserDefined ToLower]]
          DeliveryWStatus

data DeliveryEvent = 
     DeliveryEvent
     { deliveryEventEmail :: !Text,
       deliveryEventEvent :: !DeliveryWStatus,
       deliveryEventReason :: !(Maybe Text)
     }
     deriving stock (Generic, Show, Eq)
     deriving (FromJSON, ToJSON)
       via WithOptions
          '[OmitNothingFields 'True, 
            FieldLabelModifier 
            '[UserDefined FirstLetterToLower, 
              UserDefined (StripConstructor DeliveryEvent)]]
          DeliveryEvent