{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Statement.History (initTimeline, getLastRefreshTm, refreshMV) where

import BCorrespondent.Transport.Model.Frontend (HistoryDate, encodeHistoryDate)
import BCorrespondent.Statement.Dashboard (TimelineGapsItem)
import qualified Hasql.Statement as HS
import Control.Lens (rmap, dimap)
import Hasql.TH
import Hasql.Decoders (noResult)
import Hasql.Encoders (noParams)
import Data.Tuple.Extended (consT, snocT)
import Data.Int (Int64)
import Data.Tuple.Extended (mapPolyT)
import qualified Data.Vector as V
import qualified Data.Aeson as A (encode, eitherDecode)
import Data.Maybe (fromMaybe)
import Data.Text (Text)


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