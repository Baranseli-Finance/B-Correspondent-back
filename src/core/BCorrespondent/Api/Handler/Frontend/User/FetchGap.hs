{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module BCorrespondent.Api.Handler.Frontend.User.FetchGap (handle) where

import BCorrespondent.Transport.Model.Frontend (GapItemTime (..), GapItem (..), GapItemUnit (..))
import BCorrespondent.Api.Handler.Frontend.User.InitDailyBalanceSheet (mkStatus)
import BCorrespondent.Statement.Frontend (getGap, Gap (..))
import BCorrespondent.Transport.Response (Response (Error))
import BCorrespondent.Transport.Error (asError)
import BCorrespondent.Auth (AuthenticatedUser (..))
import BCorrespondent.Api.Handler.Utils (withError)
import qualified BCorrespondent.Auth as Auth
import Katip.Handler (KatipHandlerM, katipEnv, hasqlDbPool, ask)
import Data.Text (Text, pack)
import Database.Transaction (transactionM, statement)
import Control.Lens ((^.))
import Data.Functor ((<&>))

handle :: Auth.AuthenticatedUser 'Auth.Reader -> GapItemTime -> GapItemTime -> KatipHandlerM (Response GapItem)
handle AuthenticatedUser {ident, institution = Nothing} _ _ = pure $ Error (Just 403) $ asError @Text msg
  where msg = "there is no institution assigned to you. write to mailto:admin@b-correspondent.app to give the access"
handle _ from to 
  | not (validateGapItemTime from) || 
    not (validateGapItemTime to) = 
    pure $ Error (Just 400) $ asError @Text msg
  where msg = 
          "GapItemTime validation error: " <> 
          if not (validateGapItemTime from) 
          then pack (show from)
          else pack (show to)
handle AuthenticatedUser {institution = Just ident} from to = do
  hasql <- fmap (^. katipEnv . hasqlDbPool) ask
  let params = 
       (ident, 
        fromIntegral (gapItemTimeHour from), 
        fromIntegral (gapItemTimeMin from), 
        fromIntegral (gapItemTimeHour to), 
        fromIntegral (gapItemTimeMin to)
       )
  dbResp <- transactionM hasql $ statement getGap params
  pure $ withError dbResp $ \xs -> GapItem from to $ xs <&> \(Gap {..}) -> GapItemUnit gapTextualIdent $ mkStatus gapStatus

validateGapItemTime :: GapItemTime -> Bool
validateGapItemTime GapItemTime {..} = (0 <= gapItemTimeHour && gapItemTimeHour <= 12) && (0 <= gapItemTimeMin && gapItemTimeMin < 60)