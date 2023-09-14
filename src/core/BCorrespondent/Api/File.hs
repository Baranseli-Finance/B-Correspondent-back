{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.File (FileApi (..)) where

import BCorrespondent.Api.Controller.File.Download (Option)
import BCorrespondent.Auth (AuthenticatedUser, JWT)
import BCorrespondent.Transport.Id
import BCorrespondent.Transport.Response
import qualified Data.Text as T
import Servant.API.Extended
import Servant.API.Generic
import qualified Servant.Auth.Server as SA
import Servant.Multipart
import Servant.Multipart.File
import Servant.RawM

data FileApi route = FileApi
  { _fileApiUpload ::
      route
        :- Description "upload to server"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> Capture "bucket" T.Text
          :> MultipartForm Tmp Files
          :> Put '[JSON] (Response [Id "file"]),
    _fileApiPatch ::
      route
        :- Description "patch file by replacing new one"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> Capture "file_id" (Id "file")
          :> MultipartForm Tmp File
          :> Patch '[JSON] (Response ()),
    _fileApiDelete ::
      route
        :- Description "delete file"
          :> SA.Auth '[JWT] AuthenticatedUser
          :> Capture "file_id" (Id "file")
          :> Delete '[JSON] (Response ()),
    _fileApiDownload ::
      route
        :- Description "download from server"
          :> "download"
          :> Capture "user_id" (Id "user")
          :> Capture "option" Option
          :> Capture "file_id" (Id "file")
          :> QueryParam "width" Int
          :> QueryParam "height" Int
          :> RawM
  }
  deriving stock (Generic)
