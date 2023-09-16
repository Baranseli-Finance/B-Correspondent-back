{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module BCorrespondent.Api.Handler.Auth.GenerateToken (controller) where

import BCorrespondent.Transport.Model.Auth (AuthToken, InstitutionKey)
import BCorrespondent.Transport.Response (Response)
import Katip.Handler

controller :: InstitutionKey -> KatipHandlerM (Response AuthToken)
controller _ = undefined
