{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Statement.Frontend 
       (get1HourTimeline,
        getGap,
        HourTimeline (..),
        Gap (..)
       ) where

import BCorrespondent.Statement.Invoice (Status (..))
import qualified Hasql.Statement as HS
import Hasql.TH
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, encode, eitherDecode)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Control.Lens (dimap, rmap)
import qualified Data.Vector as V
import Data.Tuple.Extended (snocT)
import Data.String.Conv (toS)
import Data.Maybe (fromMaybe)
import Data.Int (Int64, Int32)

data HourTimeline = 
     HourTimeline
     { hourTimelineStartHour :: Int,
       hourTimelineStartMinute :: Int,
       hourTimelineEndHour :: Int,
       hourTimelineEndMinute :: Int,
       hourTimelineTextualIdent :: Text,
       hourTimelineStatus :: Status,
       hourTimelineIdent :: Int64,
       hourTimelineTm :: UTCTime
     }
    deriving stock (Generic)
    deriving
     (FromJSON)
     via WithOptions
          '[FieldLabelModifier
            '[CamelTo2 "_", 
              UserDefined 
              (StripConstructor 
               HourTimeline)]]
          HourTimeline

get1HourTimeline :: HS.Statement (UTCTime, UTCTime, Int64) (Either String [HourTimeline])
get1HourTimeline =
  dimap 
  (snocT (toS (show Declined)) .
   snocT (toS (show Confirmed)) .
   snocT (toS (show ForwardedToPaymentProvider)))
   (fromMaybe (Right []) . fmap (sequence . map (eitherDecode @HourTimeline . encode) . V.toList))
  [maybeStatement|
    select
      tmp.values :: jsonb[]
    from (
      select
        tm.start,
        tm.end,
        array_agg(
        json_build_object(
        'start_hour', extract(hour from tm.start),
        'start_minute', extract(minute from tm.start),
        'end_hour', extract(hour from tm.end),
        'end_minute', extract(minute from tm.end),
        'textual_ident', i.textual_view, 
        'status', i.status,
        'ident', i.id,
        'tm', cast(i.created_at as text) || 'Z')) 
        :: jsonb[] as values
      from (
        select
            el as start,
            el + interval '5min' as end
        from generate_series(
            $1 :: timestamptz,
            $2 :: timestamptz,
            interval '5 min') as _(el)
        where el < $2 :: timestamptz) as tm
      cross join (
        select 
          *
        from institution.invoice
        where institution_id = $3 :: int8) as i
      where 
      (i.status = $4 :: text or 
       i.status = $5 :: text or 
       i.status = $6 :: text) 
      and (i.appearance_on_timeline > tm.start and 
           i.appearance_on_timeline < tm.end)
      group by tm.start, tm.end) as tmp|]

data Gap = 
     Gap 
     { gapTextualIdent :: Text, 
       gapStatus :: Status,
       gapIdent :: Int64,
       gapTm :: UTCTime
     }
    deriving stock (Generic)
    deriving
     (FromJSON)
     via WithOptions
          '[FieldLabelModifier
            '[CamelTo2 "_", 
              UserDefined (StripConstructor Gap)]]
          Gap

getGap :: HS.Statement (Int64, Int32, Int32, Int32, Int32) (Either String  [Gap])
getGap = 
  rmap (sequence . map (eitherDecode @Gap . encode) . V.toList)
  [vectorStatement|
    select
      json_build_object(
        'textual_ident', textual_view, 
        'status', status,
        'ident', id,
        'tm', cast(i.created_at as text) || 'Z') :: jsonb
    from institution.invoice 
    where institution_id = $1 :: int8
    and appearance_on_timeline > 
        cast((cast(current_date as text) || ' ' ||
              cast($2 :: int as text) || ':' || 
              cast($3 :: int as text) || ':00') 
              as timestamp) 
    and appearance_on_timeline <
        cast((cast(current_date as text) || ' ' ||
              cast($4 :: int as text) || ':' || 
              cast($5 :: int as text) || ':00') 
              as timestamp)|]
 