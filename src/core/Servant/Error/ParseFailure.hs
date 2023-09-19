{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Servant.Error.ParseFailure (formatter) where

import BCorrespondent.Transport.Response (Response (Error))
import BCorrespondent.Transport.Error (addMeta, asError)
import Data.Aeson hiding (Error)
import Data.Proxy (Proxy (..))
import Data.String.Conversions (cs)
import Network.HTTP.Types (hContentType)
import Servant.API.ContentTypes (handleAcceptH)
import Servant.API.Extended (JSON)
import Servant.Server (err400, ServerError (..), errBody, errHeaders, getAcceptHeader)
import Servant.Server.Internal.ErrorFormatter
import Data.Text (Text)
import Data.String.Conv (toS)

formatter :: ErrorFormatter
formatter tr req err =
  let -- aeson Value which will be sent to the client
      value = toJSON @(Response ()) $ Error (Just 400) $ addMeta "combinator" (show tr) $ asError @Text (toS err)
      -- Accept header of the request
      accH = getAcceptHeader req
  in  -- handleAcceptH is Servant's function that checks whether the client can accept a
      -- certain message type.
      -- In this case we call it with "Proxy '[JSON]" argument, meaning that we want to return a JSON.
      case handleAcceptH (Proxy :: Proxy '[JSON]) accH value of
        -- If client can't handle JSON, we just return the body the old way
        Nothing -> err400 {errBody = cs err}
        -- Otherwise, we return the JSON formatted body and set the "Content-Type" header.
        Just (ctypeH, body) -> ServerError { errHTTPCode = 200, errBody = body, errHeaders = [(hContentType, cs ctypeH)], errReasonPhrase = mempty }
