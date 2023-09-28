{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Fs.Download (handle) where

import BCorrespondent.Transport.Id
import Katip.Handler (KatipHandlerM)
import Network.Wai (Application)

handle :: Id "user" -> Id "file" -> KatipHandlerM Application
handle _ _ = undefined
