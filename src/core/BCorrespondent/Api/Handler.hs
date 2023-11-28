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
import qualified BCorrespondent.Api.Handler.SendGrid.Mail as SendGrid.Mail
import qualified BCorrespondent.Api.Handler.SendGrid.Webhook as SendGrid.Webhook
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
import qualified BCorrespondent.Api.Handler.Frontend.User.ShiftTimelineHistory as Frontend.User.ShiftTimelineHistory
import qualified BCorrespondent.Api.Handler.Frontend.User.MakeProcuratory as Frontend.User.MakeProcuratory
import qualified BCorrespondent.Api.Handler.Frontend.User.FetchTimeline as Frontend.User.FetchTimeline
import qualified BCorrespondent.Api.Handler.Frontend.Init as Frontend.Init
import qualified BCorrespondent.Api.Handler.Webhook.CatchPaymentProvider as Webhook.CatchPaymentProvider
import qualified BCorrespondent.Api.Handler.Github.CatchWebhook as Github.CatchWebhook
import qualified BCorrespondent.Api.Handler.Fs.Upload as Fs.Upload
import qualified BCorrespondent.Api.Handler.Fs.Download as Fs.Download
import qualified BCorrespondent.Api.Handler.WS.User.Transaction as WS.User.Transaction
import qualified BCorrespondent.Api.Handler.WS.User.Wallet as WS.User.Wallet
import qualified BCorrespondent.Api.Handler.Frontend.User.GetTimelineTransaction as Frontend.User.GetTimelineTransaction
import qualified BCorrespondent.Api.Handler.Institution.RegisterWithdrawal as Institution.RegisterWithdrawal
import qualified BCorrespondent.Api.Handler.Institution.ConfirmWithdrawal as Institution.ConfirmWithdrawal
import qualified BCorrespondent.Api.Handler.Institution.InitWithdrawal as Institution.InitWithdrawal
import qualified BCorrespondent.Api.Handler.Institution.GetWithdrawalHistoryPage as Institution.GetWithdrawalHistoryPage
import qualified BCorrespondent.Api.Handler.WS.Institution.Withdrawal as WS.Institution.Withdrawal
import qualified BCorrespondent.Api.Handler.Admin.CreateUser as Admin.CreateUser
import qualified BCorrespondent.Api.Handler.Frontend.User.GetNotifications as User.GetNotifications
import qualified BCorrespondent.Api.Handler.Frontend.User.MarkNotificationRead as User.MarkNotificationRead
import qualified BCorrespondent.Api.Handler.Frontend.User.SubmitIssue as User.SubmitIssue
import qualified BCorrespondent.Api.Handler.Frontend.User.InitBalancedBook as User.InitBalancedBook
import qualified BCorrespondent.Api.Handler.Frontend.User.FetchBalancedBook as User.FetchBalancedBook
import qualified BCorrespondent.Api.Handler.WS.User.BalancedBook.Transaction as WS.User.BalancedBook.Transaction
import qualified BCorrespondent.Api.Handler.WS.User.BalancedBook.Wallet as WS.User.BalancedBook.Wallet
import qualified BCorrespondent.Api.Handler.Frontend.User.InitWorkspace as User.InitWorkspace
import qualified BCorrespondent.Api.Handler.WS.User.Notification as WS.User.Notification
-- << end handlers
import qualified BCorrespondent.Auth as Auth
import Katip
import Katip.Handler hiding (webhook, github)
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
      _httpApiFile = toServant file,
      _httpApiAdmin = toServant admin,
      _wsApi = toServant ws
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
    _foreignApiGithub = toServant github,
    _foreignApiWebhook = toServant webhook 
  }

sendgrid :: SendGridApi (AsServerT KatipHandlerM)
sendgrid =
  SendGridApi
    { _sendGridApiSendMail =
        flip logExceptionM ErrorS
          . katipAddNamespace
            (Namespace ["sendgrid", "mail"])
          . SendGrid.Mail.handle,
      _sendGridApiCatchWebhook =
        flip logExceptionM ErrorS
          . katipAddNamespace
            (Namespace ["sendgrid", "webhook"])
          . SendGrid.Webhook.catch
    }

github :: GithubApi (AsServerT KatipHandlerM)
github =
  GithubApi
    { _githubApiCatchWebhook =
        flip logExceptionM ErrorS
          . katipAddNamespace
            (Namespace ["github", "webhook"])
          . Github.CatchWebhook.catch
    }

webhook :: WebhookApi (AsServerT KatipHandlerM)
webhook =
  WebhookApi
  { _webhookApiCatchPaymentProvider = \auth kind payload ->
      auth `Auth.withAuth` \_ ->
        flip logExceptionM ErrorS $
          katipAddNamespace
          (Namespace ["webhook", "payment_provider"])
          (Webhook.CatchPaymentProvider.catch kind payload)
  }

frontend :: FrontendApi (AsServerT KatipHandlerM)
frontend =
  FrontendApi
    { _frontendApiUser =
        let nm = "frontend"
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
           (Namespace [nm, "dashboard", "timeline", "gap"])
           (Frontend.User.FetchGap.handle user from to),
    _userApiIntTimelineHistory = \auth date ->
       auth `Auth.withAuth` \user ->
         flip logExceptionM ErrorS $
           katipAddNamespace
           (Namespace [nm, "history", "timeline"]) $
           Frontend.User.IntTimelineHistory.handle user date,
    _userApiGetHourShiftTimelineHistory =
      \auth y m d direction inst hour ->
        auth `Auth.withAuth` \user ->
          flip logExceptionM ErrorS $
            katipAddNamespace
            (Namespace [nm, "history", "timeline", "shift"]) $
            Frontend.User.ShiftTimelineHistory.handle user y m d direction inst hour,
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
             (Namespace [nm, "dashboard", "timeline", "gap"])
             (Frontend.User.FetchTimeline.handle user direction point),
    _userApiGetTimelineTransaction = \auth ident ->
       auth `Auth.withAuth` \user ->
         flip logExceptionM ErrorS $
           katipAddNamespace
           (Namespace [nm,  "dashboard", "timeline", "transaction"])
           (Frontend.User.GetTimelineTransaction.handle user ident),
    _userApiGetNotifications = \auth from ->
       auth `Auth.withAuth` \user ->
         flip logExceptionM ErrorS $
           katipAddNamespace
           (Namespace [nm, "notification"]) $
           User.GetNotifications.handle user from,
    _userApiMarkNotificationRead = \auth ident ->
       auth `Auth.withAuth` \user ->
         flip logExceptionM ErrorS $
           katipAddNamespace
           (Namespace [nm, "notification", "read"]) $
           User.MarkNotificationRead.handle user ident,
    _userApiSubmitIssue = \auth req ->
       auth `Auth.withAuth` \_ ->
         flip logExceptionM ErrorS $
           katipAddNamespace
           (Namespace [nm, "issue"]) $
           User.SubmitIssue.handle req,
    _userApiInitBalancedBook = \auth ->
       auth `Auth.withAuth` \user ->
         flip logExceptionM ErrorS $
           katipAddNamespace
           (Namespace [nm, "balanced-book"]) $
           User.InitBalancedBook.handle user,
    _userApiFetchBalancedBook = \auth y m d direction ->
      auth `Auth.withAuth` \user ->
         flip logExceptionM ErrorS $
           katipAddNamespace
           (Namespace [nm, "balanced-book", "fetch"]) $
           User.FetchBalancedBook.handle user y m d direction,
    _userApiInitWorkspace = \auth ->
       auth `Auth.withAuth` \user ->
         flip logExceptionM ErrorS $
           katipAddNamespace
           (Namespace [nm, "balanced-book"]) $
           User.InitWorkspace.handle user
  }

institution :: InstitutionApi (AsServerT KatipHandlerM)
institution = 
  InstitutionApi 
  { _institutionApiInvoice = toServant invoice, 
    _institutionApiFiat = toServant $ fiat "institution" 
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

fiat :: Text -> FiatApi (AsServerT KatipHandlerM)
fiat nm = 
  FiatApi 
  { _fiatApiConfirmWithdrawal = \auth code ->
     auth `Auth.withAuth` \user ->
       flip logExceptionM ErrorS $
         katipAddNamespace
         (Namespace [nm, "fiat", "withdraw", "confirm"])
         (Institution.ConfirmWithdrawal.handle user code),
    _fiatApiRegisterWithdrawal = \auth body ->
     auth `Auth.withAuth` \user ->
       flip logExceptionM ErrorS $
         katipAddNamespace
         (Namespace [nm, "fiat", "withdraw", "confirm"])
         (Institution.RegisterWithdrawal.handle user body),
    _fiatApiInitWithdrawal = \auth ->
     auth `Auth.withAuth` \user ->
       flip logExceptionM ErrorS $
         katipAddNamespace
         (Namespace [nm, "fiat", "withdrawal", "init"]) $
         Institution.InitWithdrawal.handle user,
    _fiatApiGetWithdrawalHistoryPage = \auth page ->
     auth `Auth.withAuth` \user ->
       flip logExceptionM ErrorS $
         katipAddNamespace
         (Namespace [nm, "fiat", "withdrawal", "history", "page"]) $
         Institution.GetWithdrawalHistoryPage.handle user page,
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

admin :: AdminApi (AsServerT KatipHandlerM)
admin = 
  AdminApi
  { _adminApiCreateUser = \auth user ->
      auth `Auth.withAuth` \_ ->
       flip logExceptionM ErrorS $
         katipAddNamespace
         (Namespace ["admin", "user", "put"])
         (Admin.CreateUser.handle user)
  }

ws :: WSApi (AsServerT KatipHandlerM)
ws = 
  WSApi
  {_wsApiNotifyWithdrawRecord =
    \(pend :: WS.PendingConnection) ->
    pend `Auth.withWSAuth` \(ident, conn) ->
      flip logExceptionM ErrorS
      $ katipAddNamespace
        (Namespace ["ws", "withdraw", "item", "notify"])
        (WS.Institution.Withdrawal.handle ident conn),
    _wsApiNotifyTransactionUpdate =
      \(pend :: WS.PendingConnection) ->
        pend `Auth.withWSAuth` \(ident, conn) ->
          flip logExceptionM ErrorS
          $ katipAddNamespace
            (Namespace ["ws", "dashboard", "timeline", "transaction"])
            (WS.User.Transaction.handle ident conn),
    _wsApiNotifyWalletUpdate =
      \(pend :: WS.PendingConnection) ->
        pend `Auth.withWSAuth` \(ident, conn) ->
          flip logExceptionM ErrorS
          $ katipAddNamespace
            (Namespace ["ws", "wallet"])
            (WS.User.Wallet.handle ident conn),
    _wsApiNotifyBalancedBookTransactionAdd =
      \(pend :: WS.PendingConnection) ->
        pend `Auth.withWSAuth` \(ident, conn) ->
          flip logExceptionM ErrorS
          $ katipAddNamespace
            (Namespace ["ws", "balanced-book", "transaction"]) $
            WS.User.BalancedBook.Transaction.handle ident conn,
    _wsApiNotifyBalancedBookWalletUpdate =
      \(pend :: WS.PendingConnection) ->
        pend `Auth.withWSAuth` \(ident, conn) ->
          flip logExceptionM ErrorS
          $ katipAddNamespace
            (Namespace ["ws", "balanced-book", "wallet"]) $
            WS.User.BalancedBook.Wallet.handle ident conn,
    _wsApiNotifyNotification =
       \(pend :: WS.PendingConnection) ->
        pend `Auth.withWSAuth` \(ident, conn) ->
          flip logExceptionM ErrorS
          $ katipAddNamespace
            (Namespace ["ws", "notification"]) $
            WS.User.Notification.handle ident conn
    }