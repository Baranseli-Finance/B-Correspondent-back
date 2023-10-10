{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module BCorrespondent.Api.Handler.Frontend.User.Utils (checkInstitution) where

import qualified BCorrespondent.Auth as Auth
import BCorrespondent.Transport.Response (Response (Error))
import BCorrespondent.Transport.Error (asError)
import BCorrespondent.Auth (AuthenticatedUser (..))
import Data.Text (Text)
import Katip.Handler (KatipHandlerM)
import Data.Int (Int64)

checkInstitution :: Auth.KnownRole r => Auth.AuthenticatedUser r -> ((Int64, Int64) -> KatipHandlerM (Response resp)) -> KatipHandlerM (Response resp)
checkInstitution AuthenticatedUser {ident, institution = Nothing} _ = pure $ Error (Just 403) $ asError @Text msg
  where msg = "there is no institution assigned to you. write to mailto:admin@b-correspondent.app to give the access"
checkInstitution AuthenticatedUser {ident, institution = Just inst_ident} go = go (ident, inst_ident)
