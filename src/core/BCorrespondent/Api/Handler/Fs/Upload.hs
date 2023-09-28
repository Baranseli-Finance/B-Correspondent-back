{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Fs.Upload (handle) where

import BCorrespondent.Transport.Response (Response)
import BCorrespondent.Auth (AuthenticatedUser, Role (Writer))
import BCorrespondent.Transport.Model.Fs (Bucket)
import BCorrespondent.Transport.Id
import Katip.Handler (KatipHandlerM)
import Servant.Multipart.File

handle :: AuthenticatedUser 'Writer -> Bucket -> Files -> KatipHandlerM (Response [Id "file"])
handle _ _ _ = undefined