{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module BCorrespondent.EnvKeys where

import Control.Lens
import Data.Aeson
import Data.Aeson.Generic.DerivingVia
import qualified Data.Text as T
import GHC.Generics


data Github = 
     Github 
     { githubKey :: !T.Text, 
       githubRepos :: ![T.Text]
     }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (FromJSON)
    via WithOptions
          '[FieldLabelModifier 
            '[UserDefined ToLower, 
              UserDefined (StripConstructor Github)]]
          Github

data Telegram = 
     Telegram 
     { telegramBot :: T.Text,
       telegramChat :: T.Text,
       telegramHost :: T.Text,
       telegramEnv :: T.Text
     }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (FromJSON)
    via WithOptions
          '[FieldLabelModifier 
            '[UserDefined ToLower, 
              UserDefined (StripConstructor Telegram)]]
          Telegram

data Person = 
     Person 
     { personEmail :: !T.Text, 
       personPersonalization :: !T.Text 
     }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (FromJSON)
    via WithOptions
          '[FieldLabelModifier 
            '[UserDefined ToLower, 
              UserDefined (StripConstructor Person)]]
          Person

data Sendgrid = 
     Sendgrid 
     { sendgridUrl :: !T.Text, 
       sendgridKey :: !T.Text, 
       sendgridIdentity :: !T.Text,
       sendgridPersons :: ![Person]  
     }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (FromJSON)
    via WithOptions
          '[FieldLabelModifier 
            '[UserDefined ToLower, 
              UserDefined (StripConstructor Sendgrid)]]
          Sendgrid

data SMTP = SMTP { sMTPLogin :: !T.Text, sMTPCred :: !T.Text }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (FromJSON)
    via WithOptions
          '[FieldLabelModifier 
            '[UserDefined ToLower, 
              UserDefined (StripConstructor SMTP)]]
          SMTP

data EnvKeys = 
     EnvKeys
     { envKeysSendgrid :: !(Maybe Sendgrid),
       envKeysTelegram :: !(Maybe Telegram),
       envKeysGithub :: !(Maybe Github),
       envKeysSmtp :: !(Maybe SMTP)
     }
     deriving stock (Generic)
     deriving stock (Show)
     deriving
      (FromJSON)
      via WithOptions
            '[FieldLabelModifier 
              '[UserDefined ToLower, 
                UserDefined (StripConstructor EnvKeys)]]
            EnvKeys

makeFields ''EnvKeys
makeFields ''Sendgrid
makeFields ''Github
makeFields ''SMTP