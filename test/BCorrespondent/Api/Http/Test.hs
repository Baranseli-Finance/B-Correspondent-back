{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=16 #-}

module BCorrespondent.Api.Http.Test (spec_api) where

import Data.Proxy
import BCorrespondent.Api.Auth (AuthApi)
import BCorrespondent.Api.Invoice (InvoiceApi)
import BCorrespondent.Api.Frontend (FrontendApi)
import BCorrespondent.Transport.Model.Auth (Credentials, AuthToken, NewPassword)
import BCorrespondent.Api.Handler.SendGrid.SendMail (SendGridSendMailRequest)
import BCorrespondent.Config (Email)
import BCorrespondent.Api.Foreign.SendGrid (SendGridApi)
import BCorrespondent.Transport.Model.Frontend (ProcuratoryRequest)
import BCorrespondent.Transport.Model.Invoice (InvoiceRegisterResponse)

import Servant.API.Generic
import Servant.Swagger.Test
import Test.Hspec
import TH.Mk (mkArbitrary)

mkArbitrary ''AuthToken
mkArbitrary ''Credentials
mkArbitrary ''Email
mkArbitrary ''SendGridSendMailRequest
mkArbitrary ''NewPassword
mkArbitrary ''ProcuratoryRequest
mkArbitrary ''InvoiceRegisterResponse

spec_api :: Spec
spec_api =
  describe "Swagger spec for API" $ do
    context "ToJSON matches ToSchema (AuthApi)" $
      validateEveryToJSON (genericApi (Proxy :: Proxy AuthApi))
    context "ToJSON matches ToSchema (SendGridApi)" $
      validateEveryToJSON (genericApi (Proxy :: Proxy SendGridApi))
    context "ToJSON matches ToSchema (InvoiceApi)" $
      validateEveryToJSON (genericApi (Proxy :: Proxy InvoiceApi))
    context "ToJSON matches ToSchema (FrontendApi)" $
      validateEveryToJSON (genericApi (Proxy :: Proxy FrontendApi))