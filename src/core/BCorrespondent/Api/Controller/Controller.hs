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

module BCorrespondent.Api.Controller.Controller (controller) where

-- controllers

import BCorrespondent.Api
import qualified BCorrespondent.Api.Controller.Auth.Login as Auth.Login
import qualified BCorrespondent.Api.Controller.Auth.Logout as Auth.Logout
import qualified BCorrespondent.Api.Controller.File.Delete as File.Delete
import qualified BCorrespondent.Api.Controller.File.Download as File.Download
import qualified BCorrespondent.Api.Controller.File.Patch as File.Patch
import qualified BCorrespondent.Api.Controller.File.Upload as File.Upload
import qualified BCorrespondent.Api.Controller.SendGrid.SendMail as SendGrid.Send
import qualified BCorrespondent.Api.Controller.Auth.Email.SendLink as Auth.Email.SendLink
import qualified BCorrespondent.Api.Controller.Auth.Password.MakeLink as Auth.Password.MakeLink
import qualified BCorrespondent.Api.Controller.Auth.Password.Create as Auth.Password.Create
import qualified BCorrespondent.Auth as Auth
import Katip
import Katip.Controller hiding (webhook)
import Servant.API.Generic
import Servant.RawM.Server ()
import Servant.Server.Generic
-- import qualified Network.WebSockets.Connection as WS
import Servant.RateLimit.Server ()

controller :: Api (AsServerT KatipControllerM)
controller = Api {_apiHttp = toServant httpApi}

httpApi :: HttpApi (AsServerT KatipControllerM)
httpApi =
  HttpApi
    { _httpApiFile = toServant file,
      _httpApiAuth = toServant auth,
      _httpApiForeign = toServant _foreign
    }

file :: FileApi (AsServerT KatipControllerM)
file =
  FileApi
    { _fileApiUpload = \auth bucket files ->
        auth `Auth.withAuth` \user ->
          flip logExceptionM ErrorS $
            katipAddNamespace
              (Namespace ["file", "upload"])
              (File.Upload.controller user bucket files),
      _fileApiPatch = \auth fid file ->
        auth `Auth.withAuth` \_ ->
          flip logExceptionM ErrorS $
            katipAddNamespace
              (Namespace ["file", "patch"])
              (File.Patch.controller fid file),
      _fileApiDelete = \auth ident ->
        auth `Auth.withAuth` \_ ->
          flip logExceptionM ErrorS $
            katipAddNamespace
              (Namespace ["file", "delete"])
              (File.Delete.controller ident),
      _fileApiDownload = \userId option fid w h ->
        flip logExceptionM ErrorS $
          katipAddNamespace
            (Namespace ["file", "download"])
            (File.Download.controller userId option fid w h)
    }

auth :: AuthApi (AsServerT KatipControllerM)
auth =
  AuthApi
    { _authApiLogin = \_ cred ->
        flip logExceptionM ErrorS $
          katipAddNamespace
            (Namespace ["auth", "login"])
            (Auth.Login.controller cred),
      _authApiLogout = \auth ->
        auth `Auth.withAuth` \user ->
          flip logExceptionM ErrorS $
            katipAddNamespace
              (Namespace ["auth", "logout"])
              (Auth.Logout.controller user),
      _authApiEmailLinkSend = \auth ->
        auth `Auth.withAuth` \user ->
          flip logExceptionM ErrorS $
            katipAddNamespace
              (Namespace ["auth", "email", "link", "send"])
              (Auth.Email.SendLink.controller user),
      _authApiResetPassMakeLink = \email ->
          flip logExceptionM ErrorS $
            katipAddNamespace
              (Namespace ["auth", "pass", "reset", "link"])
              (Auth.Password.MakeLink.controller email),
      _authApiResetPassNewPass = \pass ->
          flip logExceptionM ErrorS $
            katipAddNamespace
              (Namespace ["auth", "pass", "reset"])
              (Auth.Password.Create.controller pass)
    }

_foreign :: ForeignApi (AsServerT KatipControllerM)
_foreign = ForeignApi { _foreignApiSendGrid = toServant sendgrid }

sendgrid :: SendGridApi (AsServerT KatipControllerM)
sendgrid =
  SendGridApi
    { _sendGridApiSendMail =
        flip logExceptionM ErrorS
          . katipAddNamespace
            (Namespace ["sendgrid", "send"])
          . SendGrid.Send.controller
    }