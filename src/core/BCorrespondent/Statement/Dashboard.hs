{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Statement.Dashboard
       ( getDashboard,
         get1HourTimeline,
         getGap,
         TimelineGapsItem (..),
         Dashboard (..),
         Gap (..),
         getTransaction
       ) where

import BCorrespondent.Statement.Invoice (Status (..))
import BCorrespondent.Transport.Model.Frontend (TimelineTransaction, Wallet)
import BCorrespondent.Transport.Model.Invoice (Currency)
import BCorrespondent.Statement.Types
import qualified Hasql.Statement as HS
import Hasql.TH
import Data.Aeson.Generic.DerivingVia
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, encode, eitherDecode, Value)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Control.Lens (dimap, rmap)
import qualified Data.Vector as V
import Data.Tuple.Extended (snocT, app2, app3, app4, app5)
import Data.String.Conv (toS)
import Data.Maybe (fromMaybe)
import Data.Int (Int64)
import Data.Word (Word8)
import Data.Coerce (coerce)


data TimelineGapsItem = 
     TimelineGapsItem
     { timelineGapsItemStartHour :: Int,
       timelineGapsItemStartMinute :: Int,
       timelineGapsItemEndHour :: Int,
       timelineGapsItemEndMinute :: Int,
       timelineGapsItemTextualIdent :: Text,
       timelineGapsItemStatus :: Status,
       timelineGapsItemIdent :: Int64,
       timelineGapsItemTm :: UTCTime,
       timelineGapsItemCurrency :: Currency,
       timelineGapsItemAmount :: Double
     }
    deriving stock (Generic)
    deriving
     (FromJSON)
     via WithOptions
          '[FieldLabelModifier
            '[CamelTo2 "_", 
              UserDefined 
              (StripConstructor 
               TimelineGapsItem)]]
          TimelineGapsItem

data Dashboard = 
     Dashboard 
     { dashboardInstitution :: Text,
       dashboardWallets :: [Wallet], 
       dashboardTimeline :: [TimelineGapsItem]
     }
    deriving stock (Generic)
    deriving
     (FromJSON)
     via WithOptions
          '[FieldLabelModifier
            '[CamelTo2 "_", 
              UserDefined 
              (StripConstructor 
               Dashboard)]]
          Dashboard

getDashboard :: HS.Statement (UTCTime, UTCTime, Int64) (Either String Dashboard)
getDashboard =
  dimap 
  (snocT (toS (show Declined)) .
   snocT (toS (show Confirmed)) .
   snocT (toS (show ForwardedToPaymentProvider)))
   decode
  [singletonStatement|
    with tbl as (
      select
        i.title :: text,
        gaps.xs :: jsonb[]? as gaps,
        array_agg(
          jsonb_build_object(
          'ident', iw.id,
          'currency', iw.currency,
          'amount', iw.amount,
          'walletType', iw.wallet_type)
          order by iw.wallet_type asc, iw.currency asc) 
        :: jsonb[] as wallets
      from auth.institution as i
      left join ( 
        select
          tmp.values :: jsonb[]? as xs,
          tmp.ident as ident
        from (
          select
            i.institution_id as ident,
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
            'ident', i.id,
            'tm', cast(i.appearance_on_timeline as text) || 'Z',
            'currency', i.currency,
            'amount', i.amount)
            order by i.appearance_on_timeline asc, i.transaction_textual_ident asc)
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
          and ((to_char(i.appearance_on_timeline, 'YYYY-MM-DD HH24:MI') >= to_char(tm.start, 'YYYY-MM-DD HH24:MI')) and 
               (to_char(i.appearance_on_timeline, 'YYYY-MM-DD HH24:MI') < to_char(tm.end, 'YYYY-MM-DD HH24:MI')))
          group by tm.start, tm.end, i.institution_id) as tmp) as gaps
      on i.id = gaps.ident
      inner join institution.wallet as iw
      on i.id = iw.institution_id
      where i.id = $3 :: int8
      group by i.title, gaps.xs)
     select 
      f.title :: text, 
      f.wallets :: jsonb[], 
      array_agg(s.item) filter(where s.item is not null) :: jsonb[]?
     from (select title, wallets from tbl) as f
     left join (select unnest(gaps) as item from tbl) as s on true
     group by f.title, f.wallets|]
  where
    decode (title, wallets, timeline) =
      let transform :: forall a . FromJSON a => V.Vector Value -> Either String [a]
          transform = sequence . map (eitherDecode @a . encode) . V.toList
          pair = (,) <$> transform wallets <*> fromMaybe (Right []) (fmap transform timeline)
      in fmap (uncurry (Dashboard title)) pair

get1HourTimeline :: HS.Statement (UTCTime, UTCTime, Int64) (Either String [TimelineGapsItem])
get1HourTimeline =
  dimap 
  (snocT (toS (show Declined)) .
   snocT (toS (show Confirmed)) .
   snocT (toS (show ForwardedToPaymentProvider)))
   (fromMaybe (Right []) . fmap (sequence . map (eitherDecode @TimelineGapsItem . encode) . V.toList))
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
          'ident', i.id,
          'tm', cast(i.appearance_on_timeline as text) || 'Z',
          'currency', i.currency,
          'amount', i.amount)
          order by i.appearance_on_timeline asc, i.transaction_textual_ident asc)
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
        and ((to_char(i.appearance_on_timeline, 'YYYY-MM-DD HH24:MI') >= to_char(tm.start, 'YYYY-MM-DD HH24:MI')) and 
             (to_char(i.appearance_on_timeline, 'YYYY-MM-DD HH24:MI') < to_char(tm.end, 'YYYY-MM-DD HH24:MI')))
        group by tm.start, tm.end) as tmp)
    select 
      array_agg(item) filter(where item is not null) :: jsonb[]?
    from (select unnest(items) as item from tbl) as tbl|]

data Gap = 
     Gap 
     { gapTextualIdent :: Text, 
       gapStatus :: Status,
       gapIdent :: Int64,
       gapTm :: UTCTime,
       gapCurrency :: Currency,
       gapAmount :: Double
     }
    deriving stock (Generic)
    deriving
     (FromJSON)
     via WithOptions
          '[FieldLabelModifier
            '[CamelTo2 "_", 
              UserDefined (StripConstructor Gap)]]
          Gap

getGap :: HS.Statement (Int64, Hour, Min, Hour, Min) (Either String  [Gap])
getGap = 
  dimap
  ( app5 (fromIntegral @Word8 . coerce)
  . app4 (fromIntegral @Word8 . coerce)
  . app3 (fromIntegral @Word8 . coerce)
  . app2 (fromIntegral @Word8 . coerce))
  (sequence . map (eitherDecode @Gap . encode) . V.toList)
  [vectorStatement|
    select
      json_build_object(
        'textual_ident', 
        right(
          transaction_textual_ident, 
          length(transaction_textual_ident) - 3), 
        'status', status,
        'ident', id,
        'tm', cast(appearance_on_timeline as text) || 'Z',
        'currency', currency,
        'amount', amount) :: jsonb
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
              as timestamp)
    order by appearance_on_timeline asc, transaction_textual_ident asc|]

getTransaction :: HS.Statement (Int64, Int64, Int64) (Maybe (Either String TimelineTransaction))
getTransaction = 
  rmap (fmap (eitherDecode @TimelineTransaction . encode)) 
  [singletonStatement|
    with 
      history_tr as (
        select
          case 
            when ok_transaction_sender is not null 
            then
              jsonb_build_object(
                'ok', jsonb_build_object(
                  'sender', ok_transaction_sender,
                  'senderCountry', ok_transaction_country,
                  'senderCity', ok_transaction_city,
                  'senderBank', ok_sender_bank,
                  'receiver', it.seller,
                  'receiverBank', ok_receiver_bank,
                  'amount', ok_transaction_amount,
                  'currency', ok_transaction_currency,
                  'correspondentBank', ok_correspondent_bank,
                  'charges', ok_transaction_fee,
                  'tm', ok_transaction_date || ' ' || ok_transaction_time || 'Z',
                  'description', ok_transaction_payment_description
                ),
                'failure', null)
            when failure_reason is not null 
            then 
              jsonb_build_object(
                'ok', null, 
                'failure', jsonb_build_object(
                  'reason', failure_reason,
                  'tm', cast(failure_timestamp as text) || 'Z'
                ))
            else null
          end
        from institution.invoice_and_transaction as it
        where it.invoice_ident = $3 :: int8 
        and it.user_ident = $1 :: int8),
      current_tr as (
        select 
          case 
            when ok_sender is not null
            then
              jsonb_build_object(
                'ok', jsonb_build_object(
                  'sender', ok_sender,
                  'senderCountry', ok_country,
                  'senderCity', ok_city,
                  'senderBank', ok_sender_bank,
                  'receiver', ii.seller,
                  'receiverBank', ok_receiver_bank,
                  'amount', ok_amount,
                  'currency', ok_currency,
                  'correspondentBank', ok_correspondent_bank,
                  'charges', ok_fee,
                  'tm', ok_transaction_date || ' ' || ok_transaction_time || 'Z',
                  'description', ok_description
                ),
                'failure', null)
            when failure_reason is not null
            then
              jsonb_build_object(
                'ok', null, 
                'failure', jsonb_build_object(
                  'reason', failure_reason,
                  'tm', cast(failure_timestamp as text) || 'Z'
                ))
            else null
          end    
        from institution.transaction as it
        inner join institution.invoice as ii
        on it.invoice_id = ii.id 
        where ii.institution_id = $2 :: int8 and ii.id = $3 :: int8)
    select coalesce((select * from history_tr), (select * from current_tr)) :: jsonb?|]