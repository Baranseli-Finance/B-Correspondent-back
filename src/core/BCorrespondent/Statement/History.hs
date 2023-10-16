{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Statement.History (initTimeline, getLastRefreshTm, refreshMV) where

import BCorrespondent.Transport.Model.Frontend (HistoryDate, encodeHistoryDate)
import BCorrespondent.Statement.Dashboard (TimelineGapsItem)
import qualified Hasql.Statement as HS
import Control.Lens (lmap, rmap)
import Hasql.TH
import Hasql.Decoders (noResult)
import Hasql.Encoders (noParams)

initTimeline :: HS.Statement HistoryDate (Either String [TimelineGapsItem])
initTimeline = lmap encodeHistoryDate undefined

getLastRefreshTm :: HS.Statement () Int
getLastRefreshTm = 
  rmap fromIntegral 
  [singletonStatement|
    select extract('doy' from max(refresh_time)) :: int 
    from mv.invoice_and_transaction|]

refreshMV :: HS.Statement () ()
refreshMV = HS.Statement [uncheckedSql|refresh materialized view mv.invoice_and_transaction|] noParams noResult True