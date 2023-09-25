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

module BCorrespondent.Transport.Model.Auth 
       (AuthToken (..), 
        Credentials (..), 
        AuthType, 
        InstitutionKey (..),
        NewPassword (..),
        ResetPasswordLink (..),
        AuthCode (..),
        AuthCodeHash (..),
        ResendCode (..)
       ) where

import Control.Lens
import Control.Lens.Iso.Extended (jsonb, stext)
import Data.Aeson
import Data.Aeson.Generic.DerivingVia
import Data.Proxy
import Data.Swagger
import Data.Text (Text)
import GHC.Exts
import GHC.Generics (Generic)
import Servant.API (FromHttpApiData (parseQueryParam))
import TH.Mk
import Data.Swagger.Schema.Extended 
       (deriveToSchemaFieldLabelModifier, modify, firstLetterModify)


data AuthType = JWT
  deriving stock (Generic)
  deriving (Enum)

mkToSchemaAndJSON ''AuthType
mkEnumConvertor ''AuthType
mkParamSchemaEnum ''AuthType [|isoAuthType . jsonb|]
mkFromHttpApiDataEnum ''AuthType [|from stext . from isoAuthType . to Right|]

newtype AuthToken = AuthToken Text
  deriving stock (Generic, Show)
  deriving anyclass (ToParamSchema)
  deriving newtype (ToJSON, FromJSON)

instance FromHttpApiData AuthToken where
  parseQueryParam = Right . AuthToken

instance ToSchema AuthToken where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "AuthToken") $
        toSchema (Proxy @Text)

data Credentials = 
     Credentials 
     { login :: Text, 
       password :: Text, 
       browserFp :: Text 
     }
  deriving stock (Generic, Show)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[FieldLabelModifier 
            '[UserDefined (StripConstructor Credentials)]]
          Credentials

instance ToSchema Credentials where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy @Text)
    pure $
      NamedSchema (Just "Credentials") $
        mempty
          & type_ ?~ SwaggerObject
          & properties .~ 
            fromList [
              ("login", textSchema), 
              ("password", textSchema), 
              ("browserFp", textSchema)]

data AuthCode = AuthCode { authCodeCode :: Int, authCodeHash :: Text }
  deriving stock (Generic, Show)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[FieldLabelModifier 
            '[UserDefined FirstLetterToLower, 
              UserDefined (StripConstructor AuthCode)]]
          AuthCode

deriveToSchemaFieldLabelModifier ''AuthCode [|modify (Proxy @AuthCode)|]

newtype AuthCodeHash = AuthCodeHash Text
  deriving stock (Generic)
  deriving newtype (ToJSON, FromJSON)

instance ToSchema AuthCodeHash

newtype InstitutionKey = InstitutionKey Text
  deriving stock (Generic)
  deriving anyclass (ToParamSchema)
  deriving newtype (FromHttpApiData)

data NewPassword = NewPassword { newPasswordPassword :: Text, newPasswordKey :: Text }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON)
    via WithOptions
          '[FieldLabelModifier 
            '[UserDefined FirstLetterToLower, 
              UserDefined (StripConstructor NewPassword)]]
          NewPassword

deriveToSchemaFieldLabelModifier ''NewPassword [|modify (Proxy @NewPassword)|]

newtype ResetPasswordLink = ResetPasswordLink Text
  deriving stock (Generic, Show)
  deriving newtype (FromJSON, ToJSON)

instance ToSchema ResetPasswordLink

data ResendCode = 
     ResendCode 
     { resendCodeHash :: Text, 
       resendCodeBrowserFp :: Text 
     }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON)
    via WithOptions
          '[FieldLabelModifier 
            '[UserDefined FirstLetterToLower, 
              UserDefined (StripConstructor ResendCode)]]
          ResendCode

deriveToSchemaFieldLabelModifier ''ResendCode [|firstLetterModify (Proxy @ResendCode)|]