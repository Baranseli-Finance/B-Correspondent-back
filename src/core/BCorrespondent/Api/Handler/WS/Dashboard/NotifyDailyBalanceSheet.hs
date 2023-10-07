{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module BCorrespondent.Api.Handler.WS.Dashboard.NotifyDailyBalanceSheet (handle) where

import BCorrespondent.Auth (AuthenticatedUser, Role (..))
import BCorrespondent.Transport.Model.Frontend (WSResource (WSResourceTimeline))
import BCorrespondent.Api.Handler.WS.Utils (withWS)
import Katip.Handler (KatipHandlerM)
import qualified Network.WebSockets.Connection as WS
import Data.String (fromString)
import Katip (logTM, Severity (InfoS))

handle :: AuthenticatedUser 'Reader -> WS.Connection -> WSResource -> KatipHandlerM ()
handle user conn WSResourceTimeline = withWS @Int conn $ \db _ -> $(logTM) InfoS $ fromString "ws is caught"
