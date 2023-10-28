{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.Fs (FileApi (..)) where

import BCorrespondent.Transport.Model.Fs (Bucket)
import BCorrespondent.Auth (AuthenticatedUser, JWT, Role (..))
import BCorrespondent.Transport.Id
import BCorrespondent.Transport.Response
import Servant.API.Extended
import Servant.API.Generic
import qualified Servant.Auth.Server as SA
import Servant.Multipart
import Servant.Multipart.File
import Servant.RawM (RawM)

data FileApi route = FileApi
  { _fileApiUpload ::
      route
        :- Description "upload to server"
          :> "upload" 
          :> SA.Auth '[JWT] (AuthenticatedUser 'None)
          :> Capture "bucket" Bucket
          :> MultipartForm Tmp Files
          :> Put '[JSON] (Response [Id "file"]),
    _fileApiDownload ::
      route
        :- Description "download from server"
          :> "download"
          :> Capture "user_id" (Id "user")
          :> Capture "file_id" (Id "file")
          :> RawM
  }
  deriving stock (Generic)