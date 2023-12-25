{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Statement.History (initTimeline, refreshMV, getHourShift) where

import BCorrespondent.Statement.Types
import BCorrespondent.Transport.Model.Frontend (HistoryDate, encodeHistoryDate)
import BCorrespondent.Statement.Dashboard (TimelineGapsItem)
import BCorrespondent.Statement.Invoice (Status (..))
import qualified Hasql.Statement as HS
import Control.Lens (dimap)
import Hasql.TH
import Hasql.Decoders (noResult)
import Hasql.Encoders (noParams)
import Data.Tuple.Extended (consT, snocT)
import Data.Int (Int64)
import Data.Tuple.Extended (mapPolyT, app2, app3, app4, app5, app6)
import qualified Data.Vector as V
import qualified Data.Aeson as A (encode, eitherDecode)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.String.Conv (toS)
import Data.Coerce (coerce)
import Data.Word (Word8, Word32)


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
            jsonb_build_object(
             'start_hour', extract(hour from f.start),
             'start_minute', extract(minute from f.start),
             'end_hour', extract(hour from f.end),
             'end_minute', extract(minute from f.end),
             'textual_ident', 
              right(
                s.transaction_textual_ident, 
                length(s.transaction_textual_ident) - 3), 
             'status', s.status,
             'ident', s.invoice_ident,
             'tm', cast(s.appearance_on_timeline as text) || 'Z',
             'currency', s.invoice_currency,
             'amount', s.invoice_amount)
             order by s.appearance_on_timeline asc)
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
            from institution.invoice_and_transaction
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

refreshMV :: HS.Statement () ()
refreshMV = HS.Statement [uncheckedSql|refresh materialized view institution.invoice_and_transaction|] noParams noResult True

getHourShift :: HS.Statement (Int64, Year, Month, Day, Hour, Hour, Bool) (Either String [TimelineGapsItem])
getHourShift =
  dimap
   (snocT (toS (show Declined)) .
    snocT (toS (show Confirmed)) .
    snocT (toS (show ForwardedToPaymentProvider)) .
    app2 (fromIntegral @Word32 . coerce) . 
    app3 (fromIntegral @Word8 . coerce) .
    app4 (fromIntegral @Word8 . coerce) .
    app5 (fromIntegral @Word8 . coerce) .
    app6 (fromIntegral @Word8 . coerce) )
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
          jsonb_build_object(
          'start_hour', extract(hour from tm.start),
          'start_minute', extract(minute from tm.start),
          'end_hour', extract(hour from tm.end),
          'end_minute', extract(minute from tm.end),
          'textual_ident', 
          right(
            i.transaction_textual_ident, 
            length(i.transaction_textual_ident) - 3), 
          'status', i.status,
          'ident', i.invoice_ident,
          'tm', cast(i.appearance_on_timeline as text) || 'Z',
          'currency', i.invoice_currency,
          'amount', i.invoice_amount)
          order by i.appearance_on_timeline asc)
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
            transaction_textual_ident,
            invoice_ident,
            invoice_timestamp,
            invoice_currency,
            invoice_amount,
            status,
            appearance_on_timeline
          from institution.invoice_and_transaction
          where institution_id = $1 :: int8 and not $7 :: bool 
          union
          select
            transaction_textual_ident,
            id as invoice_ident,
            created_at,
            currency as invoice_currency,
            amount as invoice_amount,
            status,
            appearance_on_timeline
          from institution.invoice
          where institution_id = $1 :: int8 and $7 :: bool) as i
        where 
        (i.status = $8 :: text or
         i.status = $9 :: text or 
         i.status = $10 :: text)
        and (i.appearance_on_timeline > tm.start)
        and (i.appearance_on_timeline < tm.end)
        group by tm.start, tm.end) as tmp)
    select
      array_agg(item) filter(where item is not null) :: jsonb[]?
    from (select unnest(items) as item from tbl) as tbl|]
