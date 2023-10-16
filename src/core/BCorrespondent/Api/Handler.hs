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

-- << start handlers
import BCorrespondent.Api
import qualified BCorrespondent.Api.Handler.SendGrid.SendMail as SendGrid.Send
import qualified BCorrespondent.Api.Handler.Auth.GenerateToken as Auth.GenerateToken
import qualified BCorrespondent.Api.Handler.Auth.Password.MakeResetLink as Auth.Password.MakeResetLink
import qualified BCorrespondent.Api.Handler.Auth.Password.New as Auth.Password.New
import qualified BCorrespondent.Api.Handler.Auth.SendAuthCode as Auth.SendAuthCode
import qualified BCorrespondent.Api.Handler.Auth.ResendAuthCode as Auth.ResendAuthCode
import qualified BCorrespondent.Api.Handler.Auth.Login as Auth.Login
import qualified BCorrespondent.Api.Handler.Auth.Logout as Auth.Logout
import qualified BCorrespondent.Api.Handler.Invoice.Register as Invoice.Register
import qualified BCorrespondent.Api.Handler.Frontend.User.InitDashboard as Frontend.User.InitDashboard
import qualified BCorrespondent.Api.Handler.Frontend.User.FetchGap as Frontend.User.FetchGap
import qualified BCorrespondent.Api.Handler.Frontend.User.IntTimelineHistory as Frontend.User.IntTimelineHistory
import qualified BCorrespondent.Api.Handler.Frontend.User.MakeProcuratory as Frontend.User.MakeProcuratory
import qualified BCorrespondent.Api.Handler.Frontend.User.FetchTimeline as Frontend.User.FetchTimeline
import qualified BCorrespondent.Api.Handler.Frontend.Init as Frontend.Init
import qualified BCorrespondent.Api.Handler.Webhook.CatchPaymentProvider as Webhook.CatchPaymentProvider
import qualified BCorrespondent.Api.Handler.Webhook.CatchGithub as Webhook.CatchGithub
import qualified BCorrespondent.Api.Handler.Fs.Upload as Fs.Upload
import qualified BCorrespondent.Api.Handler.Fs.Download as Fs.Download
import qualified BCorrespondent.Api.Handler.WS.User.Transaction as WS.User.Transaction
import qualified BCorrespondent.Api.Handler.WS.User.Wallet as WS.User.Wallet
import qualified BCorrespondent.Api.Handler.Frontend.User.GetTimelineTransaction as Frontend.User.GetTimelineTransaction
-- << end handlers
import qualified BCorrespondent.Auth as Auth
import Katip
import Katip.Handler hiding (webhook)
import Servant.API.Generic
import Servant.RawM.Server ()
import Servant.Server.Generic
import qualified Network.WebSockets.Connection as WS
import Servant.RateLimit.Server ()
import Data.Text (Text)

handler :: Api (AsServerT KatipHandlerM)
handler = Api {_apiHttp = toServant httpApi}

httpApi :: HttpApi (AsServerT KatipHandlerM)
httpApi =
  HttpApi
    { _httpApiAuth = toServant auth,
      _httpApiForeign = toServant _foreign,
      _httpApiFrontend = toServant frontend,
      _httpApiInstitution = toServant institution,
      _httpApiFile = toServant file

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
     _authApiSendAuthCode =
        flip logExceptionM ErrorS
          . katipAddNamespace
              (Namespace ["auth", "code"])
          . Auth.SendAuthCode.handle,
    _authApiResendAuthCode =
        flip logExceptionM ErrorS
          . katipAddNamespace
              (Namespace ["auth", "code", "resend"])
          . Auth.ResendAuthCode.handle,
     _authApiLogin =
      const $
        flip logExceptionM ErrorS
          . katipAddNamespace
              (Namespace ["auth", "login"])
          . Auth.Login.handle,
     _authApiLogout = \auth ->
      auth `Auth.withAuth` \user ->
        flip logExceptionM ErrorS $
          katipAddNamespace
          (Namespace ["auth", "logout"])
          (Auth.Logout.handle user)
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
  { _webhookApiCatchPaymentProvider = \kind ->
      flip logExceptionM ErrorS
        . katipAddNamespace
          (Namespace ["webhook", "payment_provider"])
        . (Webhook.CatchPaymentProvider.catch kind),
    _webhookApiCatchGithub =
      flip logExceptionM ErrorS
        . katipAddNamespace
          (Namespace ["webhook", "Github"])
        . Webhook.CatchGithub.catch
  }

frontend :: FrontendApi (AsServerT KatipHandlerM)
frontend =
  FrontendApi
    { _frontendApiUser =
        let nm = "dasboard"
        in toServant (user nm),
      _frontendApiInit =
        flip logExceptionM ErrorS
           . katipAddNamespace
             (Namespace ["frontend", "init"])
           . Frontend.Init.handle
    }

user :: Text -> UserApi (AsServerT KatipHandlerM)
user nm =
  UserApi
  { _userApiInitDashboard = \auth ->
       auth `Auth.withAuth` \user ->
         flip logExceptionM ErrorS $
           katipAddNamespace
           (Namespace [nm, "dashboard", "init"])
           (Frontend.User.InitDashboard.handle user),
    _userApiFetchGap = \auth from to ->
      auth `Auth.withAuth` \user ->
         flip logExceptionM ErrorS $
           katipAddNamespace
           (Namespace [nm, "balanceSheet", "gap"])
           (Frontend.User.FetchGap.handle user from to),
    _userApiNotifyTransactionUpdate =
      \(pend :: WS.PendingConnection) ->
        pend `Auth.withWSAuth` \(ident, conn) ->
          flip logExceptionM ErrorS
          $ katipAddNamespace
            (Namespace [nm, "balanceSheet", "transaction"])
            (WS.User.Transaction.handle ident conn),
    _userApiNotifyWalletUpdate =
      \(pend :: WS.PendingConnection) ->
        pend `Auth.withWSAuth` \(ident, conn) ->
          flip logExceptionM ErrorS
          $ katipAddNamespace
            (Namespace [nm, "wallet"])
            (WS.User.Wallet.handle ident conn),    
    _userApiIntTimelineHistory = \auth date ->
       auth `Auth.withAuth` \_ ->
         flip logExceptionM ErrorS $
           katipAddNamespace
           (Namespace [nm, "history", "timeline"]) $
           Frontend.User.IntTimelineHistory.handle date,
    _userApiMakeProcuratory = \auth req ->
       auth `Auth.withAuth` \user ->
         flip logExceptionM ErrorS $
           katipAddNamespace
             (Namespace [nm, "procuratory"])
             (Frontend.User.MakeProcuratory.handle user req),
    _userApiFetchOneHourTimeline = \auth direction point ->
       auth `Auth.withAuth` \user ->
         flip logExceptionM ErrorS $
           katipAddNamespace
             (Namespace [nm, "balanceSheet", "timeline"])
             (Frontend.User.FetchTimeline.handle user direction point),
    _userApiGetTimelineTransaction = \auth ident ->
       auth `Auth.withAuth` \user ->
         flip logExceptionM ErrorS $
           katipAddNamespace
           (Namespace [nm, "balanceSheet", "timeline", "transaction"])
           (Frontend.User.GetTimelineTransaction.handle user ident)

  }

institution :: InstitutionApi (AsServerT KatipHandlerM)
institution = 
  InstitutionApi 
  { _institutionApiInvoice = toServant invoice, 
    _institutionApiFiat = toServant fiat 
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

fiat :: FiatApi (AsServerT KatipHandlerM)
fiat = 
  FiatApi 
  { _fiatApiWithdraw = undefined, 
    _fiatApiTransactionOrder = undefined 
  }

file :: FileApi (AsServerT KatipHandlerM)
file = 
  FileApi 
  { _fileApiUpload = \auth bucket files ->
     auth `Auth.withAuth` \user ->
      flip logExceptionM ErrorS $
        katipAddNamespace
        (Namespace ["file", "upload"])
        (Fs.Upload.handle user bucket files),
   _fileApiDownload = \userIdent fileIdent ->
    flip logExceptionM ErrorS $
      katipAddNamespace
      (Namespace ["file", "download"])
      (Fs.Download.handle userIdent fileIdent) 
  }