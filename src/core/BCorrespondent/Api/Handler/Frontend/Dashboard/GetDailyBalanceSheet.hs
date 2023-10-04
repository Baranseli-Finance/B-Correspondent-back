{-# LANGUAGE DataKinds #-}

module BCorrespondent.Api.Handler.Frontend.Dashboard.GetDailyBalanceSheet (makeTextualIdent, handle) where

import BCorrespondent.Transport.Model.Invoice (Currency)
import BCorrespondent.Transport.Response (Response)
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM)
import Data.Text (Text)
import Data.Number (lengthInt)
import Data.String.Conv (toS)
import Data.Foldable (fold)

handle :: Auth.AuthenticatedUser 'Auth.Reader -> KatipHandlerM (Response ())
handle _ = undefined

makeTextualIdent :: Int -> Currency -> Text -> Text
makeTextualIdent ident _ _ = toS $ fold (replicate (9 - lengthInt ident) "0") <> show ident
