{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Statement.History (initTimeline, getLastRefreshTm, refreshMV, getHourShift) where

import BCorrespondent.Transport.Model.Frontend (HistoryDate, encodeHistoryDate)
import BCorrespondent.Statement.Dashboard (TimelineGapsItem)
import BCorrespondent.Statement.Invoice (Status (..))
import qualified Hasql.Statement as HS
import Control.Lens (rmap, dimap)
import Hasql.TH
import Hasql.Decoders (noResult)
import Hasql.Encoders (noParams)
import Data.Tuple.Extended (consT, snocT)
import Data.Int (Int64, Int32)
import Data.Tuple.Extended (mapPolyT)
import qualified Data.Vector as V
import qualified Data.Aeson as A (encode, eitherDecode)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.String.Conv (toS)


initTimeline :: HS.Statement (Int64, Int64, HistoryDate) (Either String (Text, [TimelineGapsItem]))
initTimeline = 
  dimap encode (uncurry decode)
  [singletonStatement|
    with tbl as (
      select 
        i.title :: text, 
        tbl.values :: jsonb[]?
      from auth.institution as i  
      left join (
        select
          s.institution_id as ident,
          f.start,
          f.end,
          array_agg(
            json_build_object(
             'start_hour', extract(hour from f.start),
             'start_minute', extract(minute from f.start),
             'end_hour', extract(hour from f.end),
             'end_minute', extract(minute from f.end),
             'textual_ident', s.textual_view, 
             'status', s.status,
             'ident', s.invoice_ident,
             'tm', cast(s.created_at as text) || 'Z',
             'currency', s.invoice_currency,
             'amount', s.invoice_amount))
          filter (where 
            s.appearance_on_timeline >= f.start 
            and s.appearance_on_timeline < f.end)
          :: jsonb[] as values    
        from (
          select
            el as start,
            el + interval '5min' as end
            from generate_series(
            make_timestamp($2 :: int, $3 :: int, $4 :: int, 0, 0, 0),
            make_timestamp($2 :: int, $3 :: int, $4 :: int, 1, 0, 0),
            interval '5 min') as _(el)) as f
        cross join (
          select *
            from mv.invoice_and_transaction
            where user_ident = $1 :: int8 and 
            extract('year' from appearance_on_timeline) = $2 :: int
            and extract('month' from appearance_on_timeline) = $3 :: int
            and extract('day' from appearance_on_timeline) = $4 :: int) as s
        group by s.institution_id, f.start, f.end) as tbl
        on tbl.ident = i.id 
        where tbl.values is not null)
    select
      f.title :: text,
      s.xs :: jsonb[]?
    from auth.institution as f
    left join (
      select
        f.title :: text,
        array_agg(distinct s.item) filter(where s.item is not null) :: jsonb[] as xs
      from tbl as f
      left join (select unnest(values) as item from tbl) as s on true  
      group by f.title) as s
    on f.title = s.title
    where f.id = $5 :: int8|]
  where 
    encode (user, inst, d) = snocT inst $ consT user $ mapPolyT fromIntegral $ encodeHistoryDate d
    decode title xs = fmap (title,) $ fromMaybe (Right []) $ fmap (sequence . map (A.eitherDecode @TimelineGapsItem . A.encode) . V.toList) xs

getLastRefreshTm :: HS.Statement () Int
getLastRefreshTm = 
  rmap fromIntegral 
  [singletonStatement|
    select extract('doy' from max(refresh_time)) :: int 
    from mv.invoice_and_transaction|]

refreshMV :: HS.Statement () ()
refreshMV = HS.Statement [uncheckedSql|refresh materialized view mv.invoice_and_transaction|] noParams noResult True

getHourShift :: HS.Statement (Int64, Int32, Int32, Int32, Int32, Int32) (Either String [TimelineGapsItem])
getHourShift =
  dimap
   (snocT (toS (show Declined)) .
    snocT (toS (show Confirmed)) .
    snocT (toS (show ForwardedToPaymentProvider)))
  (fromMaybe (Right []) . fmap (sequence . map (A.eitherDecode @TimelineGapsItem . A.encode) . V.toList))  
  [singletonStatement|
   with tbl as (
      select
        tmp.values :: jsonb[]? as items
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
          'ident', i.invoice_ident,
          'tm', cast(i.created_at as text) || 'Z',
          'currency', i.invoice_currency,
          'amount', i.invoice_amount))
          :: jsonb[] as values
        from (
          select
              el as start,
              el + interval '5min' as end
          from generate_series(
            make_timestamp($2 :: int, $3 :: int, $4 :: int, $5 :: int, 0, 0),
            make_timestamp($2 :: int, $3 :: int, $4 :: int, $6 :: int, 0, 0),
              interval '5 min') as _(el)) 
         as tm
        cross join (
          select 
            *
          from mv.invoice_and_transaction
          where institution_id = $1 :: int8) as i
        where 
        (i.status = $7 :: text or 
         i.status = $8 :: text or 
         i.status = $9 :: text)
        and (i.appearance_on_timeline > tm.start)
        and (i.appearance_on_timeline < tm.end)
        group by tm.start, tm.end) as tmp)
    select
      array_agg(item) filter(where item is not null) :: jsonb[]?
    from (select unnest(items) as item from tbl) as tbl|]