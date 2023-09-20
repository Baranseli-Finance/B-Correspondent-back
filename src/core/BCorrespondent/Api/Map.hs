{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Map
  ( HttpApi (..),
    module Auth,
    module Foreign,
    module Transaction,
    module Invoice
  )
where

import BCorrespondent.Api.Foreign as Foreign
import BCorrespondent.Api.Auth as Auth
import BCorrespondent.Api.Transaction as Transaction
import BCorrespondent.Api.Invoice as Invoice
import Servant.API
import Servant.API.Generic
import Servant.Swagger.Tags

data HttpApi route = HttpApi
  { _httpApiAuth ::
      route
        :- Tags "Auth"
          :> "auth"
          :> ToServant AuthApi AsApi,
    _httpApiForeign ::
      route
        :- Tags "Foreign"
          :> "foreign"
          :> ToServant ForeignApi AsApi,
    _httpApiTransaction ::
      route
        :- Tags "Transaction"
          :> "transaction"
          :> ToServant TransactionApi AsApi,
    _httpApiInvoice ::
      route
        :- Tags "Invoice"
          :> "invoice"
          :> ToServant InvoiceApi AsApi     
  }
  deriving stock (Generic)
