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

module BCorrespondent.Transport.Model.Webhook (PaymentProvider (..)) where

import Control.Lens
import Control.Lens.Iso.Extended (jsonb, stext)
import GHC.Generics (Generic)
import TH.Mk

data PaymentProvider = Elekse
  deriving stock (Generic)
  deriving (Enum)

mkToSchemaAndJSON ''PaymentProvider
mkEnumConvertor ''PaymentProvider
mkParamSchemaEnum ''PaymentProvider [|isoPaymentProvider . jsonb|]
mkFromHttpApiDataEnum ''PaymentProvider [|from stext . from isoPaymentProvider . to Right|]