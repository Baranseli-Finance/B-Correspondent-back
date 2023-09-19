{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Servant.Error.RouteNotFound404 (formatter) where

import BCorrespondent.Transport.Response (Response (Error))
import BCorrespondent.Transport.Error (asError)
import Network.Wai (Request, rawPathInfo)
import Servant.Server (err400, ServerError (..), errBody, errHeaders, getAcceptHeader)
import Data.String.Conv (toS)
import Data.Aeson (toJSON)
import Network.HTTP.Types (hContentType)
import Servant.API.ContentTypes (handleAcceptH)
import Servant.API.Extended (JSON)
import Data.Proxy (Proxy (..))
import Data.Text (Text)

formatter :: Request -> ServerError
formatter req =
  let -- aeson Value which will be sent to the client
      value = toJSON @(Response ()) $ Error (Just 404) $ asError @Text (toS ("path not found on the server: " <> rawPathInfo req))
      -- Accept header of the request
      accH = getAcceptHeader req
  in  -- handleAcceptH is Servant's function that checks whether the client can accept a
      -- certain message type.
      -- In this case we call it with "Proxy '[JSON]" argument, meaning that we want to return a JSON.
      case handleAcceptH (Proxy :: Proxy '[JSON]) accH value of
        -- If client can't handle JSON, we just return the body the old way
        Nothing -> err400 {errBody = toS ("path not found on the server: " <> rawPathInfo req) }
        -- Otherwise, we return the JSON formatted body and set the "Content-Type" header.
        Just (ctypeH, body) -> ServerError { errHTTPCode = 200, errBody = body, errHeaders = [(hContentType, toS ctypeH)], errReasonPhrase = mempty }