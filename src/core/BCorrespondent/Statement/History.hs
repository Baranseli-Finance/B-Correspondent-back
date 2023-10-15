module BCorrespondent.Statement.History (initTimeline) where

import BCorrespondent.Transport.Model.Frontend (HistoryDate, encodeHistoryDate)
import BCorrespondent.Statement.Dashboard (TimelineGapsItem)
import qualified Hasql.Statement as HS
import Control.Lens (lmap)

initTimeline :: HS.Statement HistoryDate (Either String [TimelineGapsItem])
initTimeline = lmap encodeHistoryDate undefined
