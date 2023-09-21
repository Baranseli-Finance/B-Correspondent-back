{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Api.Handler (handler) where

-- controllers

import BCorrespondent.Api
import qualified BCorrespondent.Api.Handler.SendGrid.SendMail as SendGrid.Send
import qualified BCorrespondent.Api.Handler.Auth.GenerateToken as Auth.GenerateToken
import qualified BCorrespondent.Api.Handler.Auth.Password.MakeResetLink as Auth.Password.MakeResetLink
import qualified BCorrespondent.Api.Handler.Auth.Password.New as Auth.Password.New
import qualified BCorrespondent.Api.Handler.Auth.Login as Auth.Login 
import qualified BCorrespondent.Api.Handler.Invoice.Register as Invoice.Register
import qualified BCorrespondent.Api.Handler.Frontend.GetTimeline as Frontend.GetTimeline
import qualified BCorrespondent.Api.Handler.Frontend.GetHistory as Frontend.GetHistory
import qualified BCorrespondent.Api.Handler.Frontend.MakeProcuratory as Frontend.MakeProcuratory
import qualified BCorrespondent.Api.Handler.Webhook.CatchElekse as Webhook.CatchElekse
import qualified BCorrespondent.Auth as Auth
import Katip
import Katip.Handler hiding (webhook)
import Servant.API.Generic
import Servant.RawM.Server ()
import Servant.Server.Generic
-- import qualified Network.WebSockets.Connection as WS
import Servant.RateLimit.Server ()

handler :: Api (AsServerT KatipHandlerM)
handler = Api {_apiHttp = toServant httpApi}

httpApi :: HttpApi (AsServerT KatipHandlerM)
httpApi =
  HttpApi
    { _httpApiAuth = toServant auth,
      _httpApiForeign = toServant _foreign,
      _httpApiFrontend = toServant frontend,
      _httpApiInvoice = toServant invoice
    }

auth :: AuthApi (AsServerT KatipHandlerM)
auth =
  AuthApi
    { _authApiGenerateToken = \key ->
        flip logExceptionM ErrorS $
          katipAddNamespace
            (Namespace ["auth", "institution", "token", "generate"])
            (Auth.GenerateToken.handle key),
     _authApiResetPasswordLink = \auth ->
      auth `Auth.withAuth` \user ->
         flip logExceptionM ErrorS $
           katipAddNamespace
             (Namespace ["auth", "password", "link"])
             (Auth.Password.MakeResetLink.handle user),
     _authApiResetPasswordNew = 
      const $
        flip logExceptionM ErrorS
          . katipAddNamespace
              (Namespace ["auth", "password", "new"])
          . Auth.Password.New.handle,
     _authApiLogin =
      const $
        flip logExceptionM ErrorS
          . katipAddNamespace
              (Namespace ["auth", "login"])
          . Auth.Login.handle
    }

_foreign :: ForeignApi (AsServerT KatipHandlerM)
_foreign = 
  ForeignApi 
  { _foreignApiSendGrid = toServant sendgrid,
    _foreignApiWebhook = toServant webhook 
  }

sendgrid :: SendGridApi (AsServerT KatipHandlerM)
sendgrid =
  SendGridApi
    { _sendGridApiSendMail =
        flip logExceptionM ErrorS
          . katipAddNamespace
            (Namespace ["sendgrid", "send"])
          . SendGrid.Send.handle
    }

webhook :: WebhookApi (AsServerT KatipHandlerM)
webhook =
  WebhookApi
  { _webhookApidApiCatchElekse =
      flip logExceptionM ErrorS
        . katipAddNamespace
          (Namespace ["webhook", "Elekse"])
        . Webhook.CatchElekse.catch
  }

frontend :: FrontendApi (AsServerT KatipHandlerM)
frontend =
  FrontendApi
    { _frontendApiGetDayTimeline = \auth ->
       auth `Auth.withAuth` \user ->
         flip logExceptionM ErrorS $
           katipAddNamespace
             (Namespace ["frontend", "timeline"])
             (Frontend.GetTimeline.handle user),
      _frontendApiGetHistory = \auth ->
       auth `Auth.withAuth` \_ ->
         flip logExceptionM ErrorS $
           katipAddNamespace
             (Namespace ["frontend", "history"])
             Frontend.GetHistory.handle,
     _frontendApiMakeProcuratory = \auth req ->
       auth `Auth.withAuth` \user ->
         flip logExceptionM ErrorS $
           katipAddNamespace
             (Namespace ["frontend", "procuratory"])
             (Frontend.MakeProcuratory.handle user req) 
    }

invoice :: InvoiceApi (AsServerT KatipHandlerM)
invoice = 
  InvoiceApi 
    { _invoiceApiRegister = \auth req ->
       auth `Auth.withAuth` \user ->
         flip logExceptionM ErrorS $
           katipAddNamespace
             (Namespace ["invoice", "new"])
             (Invoice.Register.handle user req)
    }