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

module BCorrespondent.Transport.Model.Institution (Withdraw, Balance (..), Balances (..)) where

import BCorrespondent.Transport.Model.Invoice (Currency)
import Data.Aeson.Generic.DerivingVia
import Data.Proxy (Proxy (..))
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier, firstLetterModify)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

data Withdraw = 
     Withdraw 
     { withdrawAmount :: !Double, 
       withdrawCurrency :: !Currency 
    }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined
              (StripConstructor 
               Withdraw)]]
      Withdraw

deriveToSchemaFieldLabelModifier ''Withdraw [|firstLetterModify (Proxy @Withdraw)|]

data Balance = 
     Balance 
     { balanceCurrency :: !Currency,  
       balanceAmount :: !Double 
     }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined
              (StripConstructor 
               Balance)]]
      Balance

deriveToSchemaFieldLabelModifier ''Balance [|firstLetterModify (Proxy @Balance)|]

data Balances = Balances { balancesXs :: [Balance] }
    deriving stock (Generic, Show)
    deriving
      (ToJSON, FromJSON)
      via WithOptions 
          '[FieldLabelModifier
            '[UserDefined FirstLetterToLower,
              UserDefined
              (StripConstructor 
               Balances)]]
      Balances

deriveToSchemaFieldLabelModifier ''Balances [|firstLetterModify (Proxy @Balances)|]