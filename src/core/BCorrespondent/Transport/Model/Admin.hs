{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Transport.Model.Admin (NewUser (..), encodeNewUser) where

import Data.Aeson.Generic.DerivingVia
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier, firstLetterModify)
import Data.Aeson (FromJSON, ToJSON)
import TH.Mk (mkEncoder, mkArbitrary)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Test.QuickCheck.Extended ()
import Database.Transaction (ParamsShow (..))

data NewUser =
     NewUser
     { newUserLogin :: Text,
       newUserEmail :: Text,
       newUserPassword :: Text
    }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined
              (StripConstructor 
               NewUser)]]
      NewUser

deriveToSchemaFieldLabelModifier ''NewUser [|firstLetterModify (Proxy @NewUser)|]

mkEncoder ''NewUser
mkArbitrary ''NewUser

encodeNewUser :: NewUser -> (Text, Text, Text)
encodeNewUser = fromMaybe undefined . mkEncoderNewUser

instance ParamsShow NewUser where
  render = show . encodeNewUser