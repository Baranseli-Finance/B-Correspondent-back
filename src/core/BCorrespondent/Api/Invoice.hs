{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Invoice (InvoiceApi (..)) where

import BCorrespondent.Transport.Model.Invoice (InvoiceRegisterRequest, InvoiceRegisterResponse)
import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Auth (AuthenticatedUser, JWT, Role (..))
import Servant.API.Extended
import Servant.API.Generic (Generic)
import qualified Servant.Auth.Server as SA

newtype InvoiceApi route = 
        InvoiceApi
        { _invoiceApiRegister ::
            route
              :- SA.Auth '[JWT] (AuthenticatedUser Writer)
                :> ReqBody '[JSON] [InvoiceRegisterRequest]
                :> Put '[JSON] (Response [InvoiceRegisterResponse])
        }
        deriving stock (Generic)