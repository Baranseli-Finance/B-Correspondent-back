{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Institution 
       (InstitutionApi (..),
        module Invoice,
        module Fiat
       ) where

import BCorrespondent.Api.Institution.Invoice as Invoice
import BCorrespondent.Api.Institution.Fiat as Fiat
import Servant.API.Extended (ToServant, AsApi, (:>), (:-))
import Servant.API.Generic (Generic)

data InstitutionApi route =
     InstitutionApi 
     { _institutionApiInvoice
        :: route 
        :- "invoice" 
        :> ToServant InvoiceApi AsApi,
      _institutionApiFiat
        :: route 
        :- "fiat" 
        :> ToServant FiatApi AsApi
     }
     deriving stock (Generic)
